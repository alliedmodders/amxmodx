; AMXJITSN.ASM: Just-In-Time compiler for the Abstract Machine of the "Pawn"
; scripting language
; (C) 1999-2000, Marc Peter; beta version; provided AS IS WITHOUT ANY WARRANTIES

; I reached >155 million instr./sec on my AMD K6-2/366 with the Hanoi "bench"
; (27 disks, no output, DOS4/GW under Win95) with this implementation of the
; JIT compiler.

; BUILD NOTE: Use nasm 0.97.x or 0.98.x with the options below.
; ----------------
; Windows  : nasm -O2 -f win32 -o amxjitsn.obj amxjitsn.asm
; Linux    : nasm -O2 -f elf -o amxjitsn.o amxjitsn.asm
; OS X     : nasm -O2 -f macho -o amxjitsn-darwin.o amxjitsn.asm
; ----------------
; If nasm 2.x must be used, replace -O2 with -O0.

; NOTE 1:
; There is only one pass implemented in this version. This means there is no
; way of knowing the size of the compiled code before it has actually been com-
; piled. So the only chance the caller has, is to count the number of opcodes
; (in amx_BrowseRelocate()) and multiply this count with a "safe" factor to
; obtain a size value big enough to hold the entire code (and data, including
; the stack and heap, after adding their sizes). Afterwards it can realloc()
; this memory block to the actually needed smaller size.

; NOTE 2:
; The compiler destroys the opcode addresses of the given source by storing the
; respective compiled code's addresses there for the final address relocation
; step.

; NOTE 3:
; Although the Pawn compiler doesn't generate the LCTRL, SCTRL and CALL.I
; instructions, I have to tell that they don't work as expected in a JIT
; compiled program, because there is no easy way of transforming AMX code
; addresses and JIT translated ones. This might be fixed in a future version.

; NOTE 4:
; Stack Pointer issues (by David Anderson)
; The JIT was changed recently so it no longer uses ESP as a general purpose
; register (GRP), because it can conflict with threading/signal systems which
; rely on the stack pointer being in-tact to find thread-ids.  My fix for this
; was to keep esp safe, but save the stack pointer in 'ecx'.  As such, ecx is no
; longer the CIP or scratch register, it is the save point for pieces of the AMX
; structure on the x86 stack.  
; This means that the optimization of the JIT has changed, as every amx stack
; push call now takes two operations instead of one (same for pop), and pushing
; addresses is 4 instructions instead of 1.
; As of this moment I don't see a better way around it, but the sacrifice for
; having pthread-safe code was deemed to be necessary.

; NOTE 5:
; NX ("No eXecute") and XD (eXecution Denied) bits
; (by Thiadmer Riemersma)
;
; AMD defined a bit "No eXecute" for the page table entries (for its 64-bit
; processors) and Intel came with the same design, but calling it differently.
; The purpose is to make "buffer overrun" security holes impossible, or at least
; very, very difficult, by marking the stack and the heap as memory regions
; such that an attempt to execute processor instructions will cause a processor
; exception (of course, a buffer overrun that is not explictly handled will then
; crash the application --instead of executing the rogue code).
;
; For JIT compilers, this has the impact that you are not allowed to execute the
; code that the JIT has generated. To do that, you must adjust the attributes
; for the memory page. For Microsoft Windows, you can use VirtualAlloc() to
; allocate a memory block with the appropriate fags; on Linux (with a recent
; kernel), you would use vmalloc_exec(). Microsoft Windows also offers the
; function VirtualProtect() to change the page attributes of an existing memory
; block, but there are caveats in its use: if the block spans multiple pages,
; these pages must be consecutive, and if there are blocks of memory in a page
; unrelated to the JIT, their page attributes will change too.
;
; The JIT compiler itself requires only read-write access (this is the default
; for a memory block that you allocate). The execution of the JIT-compiled code
; requires full access to the memory block: read, write and execute. It needs
; write access, because the SYSREQ.C opcode is patched to SYSREQ.D after the
; first lookup (this is an optimization, look up the address of the native
; function only once). For processors that do not support the NX/XD bit,
; execution of code is implicitly supported if read access is supported.
;
; During compilation, the JIT compiler requires write-access to its own code
; segment: the JIT-compiler patches P-code parameters into its own code segment
; during compilation. This is handled in the support code for amx_InitJIT.
;

; NOTE 6:
; CALLING CONVENTIONS
; (by Thiadmer Riemersma)
;
; This version is the JIT that uses the "stack calling convention". In the
; original implementation, this meant __cdecl; both for the calling convention
; for the _asm_runJIT routine itself as for the callback functions.
; The current release supports __stdcall for the callback functions; to
; use it, you need to assemble the file with STDECL defined (Since STDCALL is
; a reserved word on the assembler, I had to choose a different name for the
; macro, hence STDECL.)


; Revision History
; ----------------
; 24 february 2013 by Scott Ehlert
;       Aligned stack to 16-byte boundary for native calls in case they make library
;       calls on Mac OS X or use SSE instructions.
; 16 august 2005 by David "BAILOPAN" Anderson (DA)
;		Changed JIT to not swap stack pointer during execution.  This 
;		is playing with fire, especially with pthreads and signals on linux,
;		where the stack pointer is used to find the current thread id.  If
;		the stack pointer is altered during a thread/signal switch/interrupt
;		unexpected behaviour can occur (crashes).
; 26 july 2005 by David "BAILOPAN" Anderson (DA)
;       Fixed a bug where zero casetbl entries would crash the JIT.
; 17 february 2005  by Thiadmer Riemersms
;       Addition of the BREAK opcode, removal of the older debugging opcode
;       table. There should now be some debug support (if enabled during the
;       build of the JIT compiler), but not enough to run a debugger: the JIT
;       compiler does not keep a list that relates the code addresses of the
;       P-code versus the native code.
; 29 June 2004  by G.W.M. Vissers
;	Translated the thing into NASM. The actual generation of the code is
;	put into the data section because the code modifies itself whereas the
;	text section is usually read-only in the Unix ELF format.
;  6 march 2004  by Thiadmer Riemersma
;       Corrected a bug in OP_FILL, where a cell preceding the array would
;       be overwritten (zero'ed out). This bug was brought to my attention
;       by Robert Daniels.
; 22 december 2003  by Thiadmer Riemersma (TR)
;       Added the SYMTAG and SYSCALL.D opcodes (these are not really supported;
;       SYMTAG is a no-op).
;       Support __stdcall calling convention for for the native function "hook"
;       function (the __cdecl calling convention is also still supported).
; 14 October 2002 by Thiadmer Riemersma (TR)
;       Corrected the amx_s structure. The _hlw field was missing, which caused
;       errors for arguments to native functions that were passed by reference.
; 2002/08/05    TR
;   * store the status of the abstract machine in the AMX structure upon
;     return, so that the machine can be restarted (OP_SLEEP)
;   * added OP_NOP (for alignment, it is ignored by the JIT)
;   * make sure the JIT does not crash when we NULL is passed for the
;     return value
; 2000/03/03    MP
;       * _amx_opcodelist is equipped with an underscore, again 8-P
;       * added SRANGE as a no-op, so debugging info doesn't upset the JIT
;         compiler anymore
;       * added note about LCTRL, SCTRL and CALL.I
; 2000/03/02    MP
;       * made JIT support __cdecl calling conventions
;       * removed some rather unnecessary pops in the epilog of amx_exec_asm
;       * changed the template for CALL into a DB byte sequence (tasm 4.1
;         didn't like the immediate value)
; 1999/12/07    MP
;       * fixed crash caused by JIT compiler not saving registers
; 1999/08/06    MP - design change: closer to the "iron" with native stack
;       * The JIT compiler now generates relocatable code for case tables by
;         setting FORCERELOCATABLE = 1.
;       * removed all debug hook code
;       * exchanged meaning of ESP and ESI in asm_exec(): now low-level calls/
;         pushes/pops are possible
;       * removed the run-time functions for the CALL, CALL_I and RET op-codes,
;         they are now inline
;       * All these changes gained around 80% performance increase for the
;         hanoi bench.
; 1999/08/05    MP
;       * fixed OP_LINE in the case of NODBGCALLS==1, where no compiled address
;         was stored for the LINE byte code (i.e. SWITCH would jump to totally
;         wrong addresses). The same fix was applied to OP_FILL, OP_FILE and
;         OP_SCTRL (for the no-op case).
; 1999/08/04    MP
;       * updated with 4 new opcodes (SRANGE does nothing at the moment; 2dim.
;         arrays have not been tested.)
;       * hacked relocation code to support absoulute addresses for CASETBL
;         (This assumes that no generated address will be greater than
;         0x7fffffff. Bit no. 31 is used as flag for absolute addresses.)
;       * The run-time function for SWITCH uses a (hopefully) faster algorithm
;         to compute the destination address: It searches backwards now.
; 1999/07/08    MP - initial revision


;
; Support for the BREAK opcode (callback to the debugger): 0 = no, all other
; values = yes. Beware that the compiled code runs slower when this is enabled,
; and that debug support is still fairly minimal.
;
; GWMV: to generate LINE opcode, %define DEBUGSUPPORT
;
%undef DEBUGSUPPORT

;
; If this is set to 1 the JIT generates relocatable code for case tables, too.
; If set to 0, a faster variant for switch (using absolute addresses) is
; generated. I consider setting it to 0 a bad idea.
;
; GWMV: to use absolute addresses, %undef FORCERELOCATABLE
;
%define FORCERELOCATABLE

;
; This variable controls the generation of memory range checks at run-time.
; You should set this to 0, only when you are sure that there are no range
; violations in your Pawn programs and you really need those 5% speed gain.
;
; GWMV: To disable runtime checks, %undef it, instread of setting it to zero
;
%define DORUNTIMECHECKS

%define JIT     1
%include "amxdefn.asm"

;Registers used for JIT during execution:
;	eax		- pri
;	ebx		- reloc frame
;	ecx		- info params
;	edx		- alt
;	esi		- AMX stack
;	edi		- DAT
;	ebp		- scratch

;DA:
; These are still stored in the stack, but the stack pointer
; holding them is now kept in ecx.  
%define stk     [ecx+32]    ; define some aliases to registers that will
%define alt     [ecx+28]    ;   be stored on the stack when the code is
%define pri     [ecx+24]    ;   actually beeing executed
%define code    [ecx+20]
%define amx     [ecx+16]
%define retval  [ecx+12]
%define stp     [ecx+8]
%define hea     [ecx+4]
%define frm     [ecx]       ; FRM is NOT stored in ebp, FRM+DAT is being held
                            ; in ebx instead.

;
; #define  PUSH(v)  ( stk-=sizeof(cell), *(cell *)(data+(int)stk)=v )
;
%macro _PUSH 1
	lea		esi,[esi-4]
	mov		dword [esi], %1
%endmacro

%macro _PUSHMEM 1
	lea		esi,[esi-4]
	mov		ebp, dword %1
	mov		dword [esi], ebp
%endmacro

;
; #define  POP(v)   ( v=*(cell *)(data+(int)stk), stk+=sizeof(cell) )
;
%macro _POP 1
	mov		%1, dword [esi]
	lea		esi,[esi+4]
%endmacro


;
; For determining the biggest native code section generated for ONE Pawn
; opcode. (See the following macro and the PUBLIC function getMaxCodeSize().)
;
; GWMV: Do NOT see the following macro. See CHECKCODESIZE instead.
;
%assign MAXCODESIZE      0

;
; This is the work horse of the whole JIT: It actually copies the code.
%macro GO_ON 2-3 4
        mov     esi, %1         ;get source address of JIT code
        mov     ecx,%2-%1            ;get number of bytes to copy
        mov     [ebx],edi               ;store address for jump-correction
        add     ebx,%3
        rep     movsb
        cmp     ebx,[end_code]
        jae     code_gen_done
        jmp     dword [ebx]         ;go on with the next opcode
%endmacro

; GWMV:
; Nasm can't handle the determination of the maximum code size as was done
; in the Masm implementation, since it only does two passes. This macro is
; called *after* the code for each Pawn instruction.
%macro CHECKCODESIZE 1
	%if MAXCODESIZE < $-%1
        	%assign MAXCODESIZE $-%1
        %endif
%endmacro

;
; Modify the argument of an x86 instruction with the Pawn opcode's parameter
; before copying the code.
;
%macro putval 1
        mov     eax,[ebx+4]
        mov     dword [%1],eax
%endmacro

;
; Add an entry to the table of addresses which have to be relocated after the
; code compilation is done.
;
%macro RELOC 1-2 ;   adr, dest
        mov     ebp,[reloc_num]
        %if %0 < 2
          mov	eax,[ebx+4]
        %else
          lea	eax,[%2]
        %endif
        mov     [edx+ebp],eax           ; write absolute destination
        lea     eax,[edi+%1]
        mov     [edx+ebp+4],eax         ; write address of jump operand
        add     dword [reloc_num],8
%endmacro

%macro _DROPARGS 1              ; (TR) remove function arguments from the stack
    %ifndef STDECL               ; (for __cdecl calling convention only)
        add     esp,%1
    %endif
%endmacro

%macro  _STK_ALIGN 1            ; align stack to 16-byte boundary and
                                ; allocate %1 bytes of stack space
                                ; top of stack allocation will hold original esp
    %if %1 % 16 != 0
        %error "expected 16-byte aligned value"
    %endif

    %push stkalign
    %assign stkspace %1

    mov      ebp, esp
    and      esp, 0xFFFFFFF0
    sub      esp, %1
    mov      [esp+%1-4], ebp
%endmacro

%macro _STK_RESTORE 0           ; restore stack pointer after 16-byte alignment
    %ifnctx stkalign
        %fatal "expected _STK_ALIGN before _STK_RESTORE"
    %endif

    mov      esp, [esp+stkspace-4]

    %pop
%endmacro

global  asm_runJIT, _asm_runJIT
global  amx_exec_jit, _amx_exec_jit
global  getMaxCodeSize, _getMaxCodeSize


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                             ;
; void   asm_runJIT( AMX_HEADER *amxh, JumpAddressArray *jumps, void *dest )  ;
;                                eax                     edx          ebx     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; asm_runJIT() assumes that the code of this module is allready browsed and
; relocated for the JIT compiler. It also assumes that both the jumps array and
; the dest memory block are large enough to hold all the data it has to write
; to them, as well as that the prefix (header) has already been copied to dest.

asm_runJIT:
_asm_runJIT:
        push    ebp
        push    ebx
        push    edi
        push    esi

        mov     eax,[esp+20]            ; get amxh
        mov     edx,[esp+24]            ; get jumps array
        mov     ebx,[esp+28]            ; get destination

        mov     [amxhead],eax           ; save pointer to AMX_HEADER struct
        mov     ecx,[eax+_cod]          ; get offset of start of code
        mov     eax,[eax+_dat]          ; offset of start of data = end of code
        mov     edi,ecx
        add     ecx,[amxhead]           ; compute the real pointer
        add     eax,[amxhead]           ; dito
        add     edi,ebx                 ; get write pointer into EDI
        mov     [compiled_code],ebx
        mov     [end_code],eax          ; Store end-of-code address, so JIT
                                        ; compiler knows when to stop.
        mov     dword [reloc_num],0 ; init the index into the jumps array

        mov     ebx,ecx
        jmp     dword [ecx]         ; start compiling

        ; The compiler will jump back here when code generation is complete.

code_gen_done:                          ; Now copy the data section.
        mov     ebp,[amxhead]           ; get source AMX_HEADER start address
        add     edi,3                   ; DAT follows directly after COD
        and     edi,0fffffffch          ; align it on a DWORD boundary
        push    edi                     ; save data start pointer
        mov     esi,[end_code]          ; get start of data segment
        mov     ecx,[ebp+_h_hea]
        sub     ecx,[ebp+_dat]          ; compute length of array to copy
        rep movsb                       ; copy the data

        ; Now adjust the register values in the compiled AMX_HEADER.
        ; COD stays the same since the size of AMX_HEADER doesn't change in
        ; compiled mode.
        mov     ebx,[compiled_code]     ; get compiled AMX's header address
        pop     esi                     ; recall data start pointer
        sub     esi,ebx                 ; DAT = size of code + size of prefix
        mov     [ebx+_dat],esi          ; write corrected DAT register

        ;HEA and STP are already relative to DAT, so we don't need to fix them.

        ; Now the calls/jumps in the compiled code have to be relocated.
        sub     ecx,ecx         ; reset offset into relocation table
        cmp     ecx,[reloc_num]
        jae     reloc_code_done ; if there's nothing to fix, skip this part
    reloc_code_loop:
        mov     eax,[edx+ecx]   ; get destination address
        mov     edi,[edx+ecx+4] ; determine where to write the relocated value
        add     ecx,8           ; set pointer to next entry in relocation table
        add     edi,4           ; base address from where the offset is taken
%ifndef FORCERELOCATABLE
        ;MP: hack to suport absolute addresses for the CASETBL instruction
        test    eax,80000000h   ; check whether it is an absolute address
        pushf
        and     eax,7fffffffh   ; clear the flag bit for absolute addresses
        popf
        mov     eax,[eax]       ; translate into compiled absolute address
        jne     write_reloc     ; leave out the subtraction if absolute
%else
        mov     eax,[eax]       ; translate into compiled absolute address
%endif
        sub     eax,edi         ; make a relative offset
      write_reloc:
        mov     [edi-4],eax     ; write the relocated address
        cmp     ecx,[reloc_num]
        jb      reloc_code_loop

reloc_code_done:
        ; Relocate the addresses in the AMX_HEADER structure. (CIP and publics)
        add     ebp,[ebp+_cod]  ; make all addresses relative to COD, not base
        mov     eax,[ebx+_h_cip]
        add     eax,ebp         ; get absolute source CIP
        mov     eax,[eax]       ; translate CIP to compiled address
        sub     eax,ebx         ; make it relative to base
        sub     eax,[ebx+_cod]  ; and now relative to COD
        mov     [ebx+_h_cip],eax; store relocated CIP
        mov     edi,[ebx+_publics]
        sub     esi,esi
        mov     ecx,[ebx+_natives]
        sub     ecx,edi         ; ECX = _natives - _publics = public table size
        mov     si,[ebx+_defsize]
        or      ecx,ecx
        jz      reloc_done      ; If there are no publics, we are done.
    reloc_publics_loop:
        mov     eax,[ebx+edi]   ; get public function offset
        add     eax,ebp         ; make it a source address
        mov     eax,[eax]       ; translate to compiled address
        sub     eax,ebx         ; make it an offset relative to base
        sub     eax,[ebx+_cod]  ; and now relative to COD
        mov     [ebx+edi],eax   ; write corrected address back
        add     edi,esi         ; step to next public function entry
        sub     ecx,esi
        ja      reloc_publics_loop

reloc_done:
        mov     eax,0
        pop     esi
        pop     edi
        pop     ebx
        pop     ebp
        ret

OP_LOAD_PRI:
;nop;
        putval  j_load_pri+2
        GO_ON   j_load_pri, OP_LOAD_ALT, 8

        j_load_pri:
        mov     eax,[edi+12345678h]
	CHECKCODESIZE j_load_pri

OP_LOAD_ALT:
;nop;
        putval  j_load_alt+2
        GO_ON   j_load_alt, OP_LOAD_S_PRI, 8

        j_load_alt:
        mov     edx,[edi+12345678h]
	CHECKCODESIZE j_load_alt

;good
OP_LOAD_S_PRI:
;nop;
        putval  j_load_s_pri+2
        GO_ON   j_load_s_pri, OP_LOAD_S_ALT, 8

        j_load_s_pri:
        mov     eax,[ebx+12345678h]
	CHECKCODESIZE j_load_s_pri

;good
OP_LOAD_S_ALT:
;nop;
        putval  j_load_s_alt+2
        GO_ON   j_load_s_alt, OP_LOAD_I, 8

        j_load_s_alt:
        mov     edx,[ebx+12345678h]
	CHECKCODESIZE j_load_s_alt

OP_LOAD_I:
;nop;
        GO_ON   j_load_i, OP_LODB_I

        j_load_i:
%ifdef DORUNTIMECHECKS
        call    [verify_adr_eax]
%endif
        mov     eax,[edi+eax]
	CHECKCODESIZE j_load_i

OP_LODB_I:
;nop;
        mov     eax,[ebx+4]
        mov     eax,dword [(lodb_and-4)+eax*4]
        mov     dword [j_lodb_i_sm+1],eax   ;modify AND instruction
        GO_ON   j_lodb_i, OP_LREF_PRI, 8

        j_lodb_i:
%ifdef DORUNTIMECHECKS
        call    [verify_adr_eax]
%endif
        mov     eax,[edi+eax]           ;subject to misalignment stalls
        j_lodb_i_sm:
        and     eax,12345678h
	CHECKCODESIZE j_lodb_i

OP_LREF_PRI:
;nop;
        putval  j_lref_pri+2
        GO_ON   j_lref_pri, OP_LREF_ALT, 8

        j_lref_pri:
        mov     eax,[edi+12345678h]
        mov     eax,[edi+eax]
	CHECKCODESIZE j_lref_pri

OP_LREF_ALT:
;nop;
        putval  j_lref_alt+2
        GO_ON   j_lref_alt, OP_LREF_S_PRI, 8

        j_lref_alt:
        mov     edx,[edi+12345678h]
        mov     edx,[edi+edx]
	CHECKCODESIZE j_lref_alt

OP_LREF_S_PRI:
;nop;
        putval  j_lref_s_pri+2
        GO_ON   j_lref_s_pri, OP_LREF_S_ALT, 8

        j_lref_s_pri:
        mov     eax,[ebx+12345678h]
        mov     eax,[edi+eax]
	CHECKCODESIZE j_lref_s_pri

OP_LREF_S_ALT:
;nop;
        putval  j_lref_s_alt+2
        GO_ON   j_lref_s_alt, OP_CONST_PRI, 8

        j_lref_s_alt:
        mov     edx,[ebx+12345678h]
        mov     edx,[edi+edx]
	CHECKCODESIZE j_lref_s_alt

;good
OP_CONST_PRI:
;nop;
        putval  j_const_pri+1
        GO_ON   j_const_pri, OP_CONST_ALT, 8

        j_const_pri:
        mov     eax,12345678h
	CHECKCODESIZE j_const_pri

;good
OP_CONST_ALT:
;nop;
        putval  j_const_alt+1
        GO_ON   j_const_alt, OP_ADDR_PRI, 8

        j_const_alt:
        mov     edx,12345678h
	CHECKCODESIZE j_const_alt

;good
OP_ADDR_PRI:
;nop;
        putval  j_addr_pri+1
        GO_ON   j_addr_pri, OP_ADDR_ALT, 8

        j_addr_pri:
        mov     eax,12345678h
        add     eax,frm
	CHECKCODESIZE j_addr_pri

;good
OP_ADDR_ALT:
;nop;
        putval  j_addr_alt+1
        GO_ON   j_addr_alt, OP_STOR_PRI, 8

        j_addr_alt:
        mov     edx,12345678h
        add     edx,frm
	CHECKCODESIZE j_addr_alt

OP_STOR_PRI:
;nop;
        putval  j_stor_pri+2
        GO_ON   j_stor_pri, OP_STOR_ALT, 8

        j_stor_pri:
        mov     [edi+12345678h],eax
	CHECKCODESIZE j_stor_pri

OP_STOR_ALT:
;nop;
        putval  j_stor_alt+2
        GO_ON   j_stor_alt, OP_STOR_S_PRI, 8

        j_stor_alt:
        mov     [edi+12345678h],edx
	CHECKCODESIZE j_stor_alt

;good
OP_STOR_S_PRI:
;nop;
        putval  j_stor_s_pri+2
        GO_ON   j_stor_s_pri, OP_STOR_S_ALT, 8

        j_stor_s_pri:
        mov     [ebx+12345678h],eax
	CHECKCODESIZE j_stor_s_pri

;good
OP_STOR_S_ALT:
;nop;
        putval  j_stor_s_alt+2
        GO_ON   j_stor_s_alt, OP_STOR_I, 8

        j_stor_s_alt:
        mov     [ebx+12345678h],edx
	CHECKCODESIZE j_stor_s_alt

;good
OP_STOR_I:
;nop;
        GO_ON   j_stor_i, OP_STRB_I

        j_stor_i:
%ifdef DORUNTIMECHECKS
        call    [verify_adr_edx]
%endif
        mov     [edi+edx],eax
	CHECKCODESIZE j_stor_i

OP_STRB_I:
;nop;
        mov     eax,[ebx+4]
        cmp     eax,1
        jne     strb_not1byte
        GO_ON   j_strb_i_1b, strb_not1byte, 8
        j_strb_i_1b:
%ifdef DORUNTIMECHECKS
        call    [verify_adr_edx]
%endif
        mov     [edi+edx],al
        CHECKCODESIZE j_strb_i_1b

    strb_not1byte:
        cmp     eax,4
        je      strb_4byte
        GO_ON   j_strb_i_2b, strb_4byte, 8
        j_strb_i_2b:
%ifdef DORUNTIMECHECKS
        call    [verify_adr_edx]
%endif
        mov     [edi+edx],ax
        CHECKCODESIZE j_strb_i_2b

    strb_4byte:
        GO_ON   j_strb_i_4b, OP_SREF_PRI, 8
        j_strb_i_4b:
%ifdef DORUNTIMECHECKS
        call    [verify_adr_edx]
%endif
        mov     [edi+edx],eax
	CHECKCODESIZE j_strb_i_4b

OP_SREF_PRI:
;nop;
        putval  j_sref_pri+2
        GO_ON   j_sref_pri, OP_SREF_ALT, 8

        j_sref_pri:
        mov     ebp,[edi+12345678h]
        mov     [edi+ebp],eax
	CHECKCODESIZE j_sref_pri

OP_SREF_ALT:
;nop;
        putval  j_sref_alt+2
        GO_ON   j_sref_alt, OP_SREF_S_PRI, 8

        j_sref_alt:
        mov     ebp,[edi+12345678h]
        mov     [edi+ebp],edx
	CHECKCODESIZE j_sref_alt

OP_SREF_S_PRI:
;nop;
        putval  j_sref_s_pri+2
        GO_ON   j_sref_s_pri, OP_SREF_S_ALT, 8

        j_sref_s_pri:
        mov     ebp,[ebx+12345678h]
        mov     [edi+ebp],eax
	CHECKCODESIZE j_sref_s_pri

OP_SREF_S_ALT:
;nop;
        putval  j_sref_s_alt+2
        GO_ON   j_sref_s_alt, OP_LIDX, 8

        j_sref_s_alt:
        mov     ebp,[ebx+12345678h]
        mov     [edi+ebp],edx
	CHECKCODESIZE j_sref_s_alt

;good
OP_LIDX:
;nop;
        GO_ON   j_lidx, OP_LIDX_B

        j_lidx:
        lea     eax,[edx+4*eax]
%ifdef DORUNTIMECHECKS
        call    [verify_adr_eax]
%endif
        mov     eax,[edi+eax]
	CHECKCODESIZE j_lidx

OP_LIDX_B:
;nop;
        mov     al,[ebx+4]
        mov     byte [j_lidx_b+2],al
        GO_ON   j_lidx_b, OP_IDXADDR, 8

        j_lidx_b:
        shl     eax,12h
        add     eax,edx
%ifdef DORUNTIMECHECKS
        call    [verify_adr_eax]
%endif
        mov     eax,[edi+eax]
	CHECKCODESIZE j_lidx_b

;good
OP_IDXADDR:
;nop;
        GO_ON   j_idxaddr, OP_IDXADDR_B

        j_idxaddr:
        lea     eax,[edx+4*eax]
	CHECKCODESIZE j_idxaddr

OP_IDXADDR_B:
;nop;
        mov     al,[ebx+4]
        mov     byte [j_idxaddr_b+2],al
        GO_ON   j_idxaddr_b, OP_ALIGN_PRI, 8

        j_idxaddr_b:
        shl     eax,12h
        add     eax,edx
	CHECKCODESIZE j_idxaddr_b

OP_ALIGN_PRI:
;nop;
        mov     eax,4
        sub     eax,[ebx+4]
        mov     dword [j_align_pri+1],eax
        GO_ON   j_align_pri, OP_ALIGN_ALT, 8

        j_align_pri:
        xor     eax,12345678h
	CHECKCODESIZE j_align_pri

OP_ALIGN_ALT:
;nop;
        mov     eax,4
        sub     eax,[ebx+4]
        mov     dword [j_align_alt+1],eax
        GO_ON   j_align_alt, OP_LCTRL, 8

        j_align_alt:
        xor     edx,12345678h
	CHECKCODESIZE j_align_alt

OP_LCTRL:
;nop;
        mov     eax,[ebx+4]
        cmp     eax,0
        jne     lctrl_1
        GO_ON   j_lctrl_0, lctrl_1, 8
        j_lctrl_0:
        mov     eax,code ; 1=COD
    	CHECKCODESIZE j_lctrl_0
    lctrl_1:
        cmp     eax,1
        jne     lctrl_2
        GO_ON   j_lctrl_1, lctrl_2, 8
        j_lctrl_1:
        mov     eax,edi  ; 1=DAT
    	CHECKCODESIZE j_lctrl_1
    lctrl_2:
        cmp     eax,2
        jne     lctrl_3
        GO_ON   j_lctrl_2, lctrl_3, 8
        j_lctrl_2:
        mov     eax,hea  ; 2=HEA
    	CHECKCODESIZE j_lctrl_2
    lctrl_3:
        cmp     eax,3
        jne     lctrl_4
        GO_ON   j_lctrl_3, lctrl_4, 8
        j_lctrl_3:
        mov     ebp,amx
        mov     eax,[ebp+_stp]
    	CHECKCODESIZE j_lctrl_3
    lctrl_4:
        cmp     eax,4
        jne     lctrl_5
        GO_ON   j_lctrl_4, lctrl_5, 8
        j_lctrl_4:
        mov     eax,esi         ; 4=STK
        sub     eax,edi
    	CHECKCODESIZE j_lctrl_4
    lctrl_5:
        cmp     eax,5
        jne     lctrl_6
        GO_ON   j_lctrl_5, lctrl_6, 8
        j_lctrl_5:
        mov     eax,frm         ; 5=FRM
    	CHECKCODESIZE j_lctrl_5
    lctrl_6:
        mov     dword [j_lctrl_6+1],edi
        GO_ON   j_lctrl_6, OP_SCTRL, 8
        j_lctrl_6:
        mov     eax,12345678h   ; 6=CIP
	CHECKCODESIZE j_lctrl_6


OP_SCTRL:
;nop;
        mov     eax,[ebx+4]
        cmp     eax,2
        jne     sctrl_4
        GO_ON   j_sctrl_2, sctrl_4, 8
        j_sctrl_2:
        mov     hea,eax  ; 2=HEA
    	CHECKCODESIZE j_sctrl_2
    sctrl_4:
        cmp     eax,4
        jne     sctrl_5
        GO_ON   j_sctrl_4, sctrl_5, 8
        j_sctrl_4:
        ;mov     esp,eax  ; 4=STK
        ;add    esp,edi  ; relocate stack
        lea     esi,[eax+edi]
    	CHECKCODESIZE j_sctrl_4
    sctrl_5:
        cmp     eax,5
        jne     sctrl_ignore
        GO_ON   j_sctrl_5, sctrl_ignore, 8
        j_sctrl_5:
        mov     ebx,eax  ; 5=FRM
        mov     frm,eax
        add     ebx,edi  ; relocate frame
    	CHECKCODESIZE j_sctrl_5
    sctrl_ignore:
        mov     [ebx],edi
        add     ebx,8
        jmp     dword [ebx]

OP_MOVE_PRI:
;nop;
        GO_ON   j_move_pri, OP_MOVE_ALT

        j_move_pri:
        mov     eax,edx
	CHECKCODESIZE j_move_pri

;good
OP_MOVE_ALT:
;nop;
        GO_ON   j_move_alt, OP_XCHG

        j_move_alt:
        mov     edx,eax
	CHECKCODESIZE j_move_alt

OP_XCHG:
;nop;
        GO_ON   j_xchg, OP_PUSH_PRI

        j_xchg:                 ;one might use pushes/pops for pre-586's
        xchg    eax,edx
	CHECKCODESIZE j_xchg

;good
OP_PUSH_PRI:
;nop;
        GO_ON   j_push_pri, OP_PUSH_ALT

        j_push_pri:
        _PUSH   eax
	CHECKCODESIZE j_push_pri

;good
OP_PUSH_ALT:
;nop;
        GO_ON   j_push_alt, OP_PUSH_R_PRI

        j_push_alt:
        _PUSH   edx
	CHECKCODESIZE j_push_alt

OP_PUSH_R_PRI:
;nop;
        putval  j_push_r_pri+2
        GO_ON   j_push_r_pri, OP_PUSH_C, 8

    j_push_r_pri:
    	push 	ecx
        mov     ecx,12345678h
        j_push_loop:
        _PUSH   eax
        loop    j_push_loop
        pop		ecx
        ;dec     ecx
        ;jnz     j_push_loop
	CHECKCODESIZE j_push_r_pri

;good
OP_PUSH_C:
;nop;
        putval  j_push_c_end-4
        GO_ON   j_push_c, OP_PUSH, 8

    j_push_c:
        _PUSH   12345678h
    j_push_c_end:
	CHECKCODESIZE j_push_c

OP_PUSH:
;nop;
        putval  j_push_end-6
        GO_ON   j_push, OP_PUSH_S, 8

    j_push:
        _PUSHMEM   [edi+12345678h]
    j_push_end:
	CHECKCODESIZE j_push

;good
OP_PUSH_S:
;nop;
        putval  j_push_s_end-6
        GO_ON   j_push_s, OP_POP_PRI, 8

        j_push_s:
        _PUSHMEM   [ebx+12345678h]
        j_push_s_end:
	CHECKCODESIZE j_push_s

OP_POP_PRI:
;nop;
        GO_ON   j_pop_pri, OP_POP_ALT

        j_pop_pri:
        _POP    eax
	CHECKCODESIZE j_pop_pri

;good
OP_POP_ALT:
;nop;
        GO_ON   j_pop_alt, OP_STACK

        j_pop_alt:
        _POP    edx
	CHECKCODESIZE j_pop_alt

;good
OP_STACK:
;nop;
        putval  j_stack+4
        GO_ON   j_stack, OP_HEAP, 8

        j_stack:
        mov     edx,esi
        add     esi,12345678h
        sub     edx,edi
%ifdef DORUNTIMECHECKS
        call    [chk_marginstack]
%endif
	CHECKCODESIZE j_stack

;good
OP_HEAP:
;nop;
        putval  j_heap_call-4
        GO_ON   j_heap, OP_PROC, 8

        j_heap:
        mov     edx,hea
        add     dword hea,12345678h
        j_heap_call:
%ifdef DORUNTIMECHECKS
        call    [chk_marginheap]
%endif
	CHECKCODESIZE j_heap

;good
OP_PROC:
;nop;
        GO_ON   j_proc, OP_RET

        j_proc:                 ;[STK] = FRM, STK = STK - cell size, FRM = STK
        _PUSHMEM   frm          ; push old frame (for RET/RETN)
        mov     frm,esi         ; get new frame
        mov     ebx,esi         ; already relocated
        sub     frm,edi         ; relocate frame
	CHECKCODESIZE j_proc

OP_RET:
;nop;
        GO_ON   j_ret, OP_RETN

        j_ret:
        _POP    ebx             ; pop frame
        lea		esi,[esi+4]
        mov     frm,ebx
        add     ebx,edi
        ret
        ;call   [jit_ret]
	CHECKCODESIZE j_ret

;good
OP_RETN:
;nop;
        GO_ON   j_retn, OP_CALL

        j_retn:
        jmp     [jit_retn]
	CHECKCODESIZE j_retn

;good
OP_CALL:
;nop;
        RELOC   j_call_e8-j_call+1
        GO_ON   j_call, OP_CALL_I, 8

        j_call:
        ;call   12345678h ; tasm chokes on this out of a sudden
        _PUSH	0
        j_call_e8:
        db      0e8h, 0, 0, 0, 0
	CHECKCODESIZE j_call

OP_CALL_I:
;nop;
        GO_ON   j_call_i, OP_JUMP

        j_call_i:
        _PUSH	0
        call    eax
	CHECKCODESIZE j_call_i

;good
OP_JUMP:
;nop;
        RELOC   1
        GO_ON   j_jump, OP_JREL, 8

        j_jump:
        DB      0e9h
        DD      12345678h
	CHECKCODESIZE j_jump

OP_JREL:
;nop;
        mov     eax,[ebx+4]
        ; create an absolute address from the relative one
        RELOC   1, eax+ebx+8
        ; GWMV: is the next line really correct!?
        GO_ON   j_jump, OP_JREL, 8

;good
OP_JZER:
;nop;
        RELOC   4
        GO_ON   j_jzer, OP_JNZ, 8

    j_jzer:
        or      eax,eax
        DB      0fh, 84h, 0, 0, 0, 0    ;jz NEAR 0      (tasm sucks a bit)
	CHECKCODESIZE j_jzer

;good
OP_JNZ:
;nop;
        RELOC   4
        GO_ON   j_jnz, OP_JEQ, 8

    j_jnz:
        or      eax,eax
        DB      0fh, 85h, 0, 0, 0, 0    ;jnz NEAR 0
	CHECKCODESIZE j_jnz

;good
OP_JEQ:
;nop;
        RELOC   4
        GO_ON   j_jeq, OP_JNEQ, 8

    j_jeq:
        cmp     eax,edx
        DB      0fh, 84h, 0, 0, 0, 0    ;je NEAR 0      (tasm sucks a bit)
	CHECKCODESIZE j_jeq

OP_JNEQ:
;nop;
        RELOC   4
        GO_ON   j_jneq, OP_JLESS, 8

    j_jneq:
        cmp     eax,edx
        DB      0fh, 85h, 0, 0, 0, 0    ;jne NEAR 0     (tasm sucks a bit)
	CHECKCODESIZE j_jneq

OP_JLESS:
;nop;
        RELOC   4
        GO_ON   j_jless, OP_JLEQ, 8

    j_jless:
        cmp     eax,edx
        DB      0fh, 82h, 0, 0, 0, 0    ;jb NEAR 0      (tasm sucks a bit)
	CHECKCODESIZE j_jless

OP_JLEQ:
;nop;
        RELOC   4
        GO_ON   j_jleq, OP_JGRTR, 8

    j_jleq:
        cmp     eax,edx
        DB      0fh, 86h, 0, 0, 0, 0    ;jbe NEAR 0     (tasm sucks a bit)
	CHECKCODESIZE j_jleq

OP_JGRTR:
;nop;
        RELOC   4
        GO_ON   j_jgrtr, OP_JGEQ, 8

    j_jgrtr:
        cmp     eax,edx
        DB      0fh, 87h, 0, 0, 0, 0    ;ja NEAR 0      (tasm sucks a bit)
	CHECKCODESIZE j_jgrtr

OP_JGEQ:
;nop;
        RELOC   4
        GO_ON   j_jgeq, OP_JSLESS, 8

    j_jgeq:
        cmp     eax,edx
        DB      0fh, 83h, 0, 0, 0, 0    ;jae NEAR 0 (unsigned comparison)
	CHECKCODESIZE j_jgeq

OP_JSLESS:
;nop;
        RELOC   4
        GO_ON   j_jsless, OP_JSLEQ, 8

    j_jsless:
        cmp     eax,edx
        DB      0fh, 8ch, 0, 0, 0, 0    ;jl NEAR 0
	CHECKCODESIZE j_jsless

;good
OP_JSLEQ:
;nop;
        RELOC   4
        GO_ON   j_jsleq, OP_JSGRTR, 8

    j_jsleq:
        cmp     eax,edx
        DB      0fh, 8eh, 0, 0, 0, 0    ;jle NEAR 0
	CHECKCODESIZE j_jsleq

OP_JSGRTR:
;nop;
        RELOC   4
        GO_ON   j_jsgrtr, OP_JSGEQ, 8

    j_jsgrtr:
        cmp     eax,edx
        DB      0fh, 8Fh, 0, 0, 0, 0    ;jg NEAR 0
	CHECKCODESIZE j_jsgrtr

OP_JSGEQ:
;nop;
        RELOC   4
        GO_ON   j_jsgeq, OP_SHL, 8

    j_jsgeq:
        cmp     eax,edx
        DB      0fh, 8dh, 0, 0, 0, 0    ;jge NEAR 0
	CHECKCODESIZE j_jsgeq

OP_SHL:
;nop;
        GO_ON   j_shl, OP_SHR
   j_shl:
        push	ecx   
        mov     ecx,edx
        shl     eax,cl
        pop		ecx
	CHECKCODESIZE j_shl

OP_SHR:
;nop;
        GO_ON   j_shr, OP_SSHR
   j_shr:
        push	ecx
        mov     ecx,edx
        shr     eax,cl
        pop		ecx
	CHECKCODESIZE j_shr

OP_SSHR:
;nop;
        GO_ON   j_sshr, OP_SHL_C_PRI
   j_sshr:
        push	ecx
        mov     ecx,edx
        sar     eax,cl
        pop		ecx
	CHECKCODESIZE j_sshr

OP_SHL_C_PRI:
;nop;
        mov     al,[ebx+4]
        mov     byte [j_shl_c_pri+2],al
        GO_ON   j_shl_c_pri, OP_SHL_C_ALT, 8
    j_shl_c_pri:
        shl     eax,12h
	CHECKCODESIZE j_shl_c_pri

OP_SHL_C_ALT:
;nop;
        mov     al,[ebx+4]
        mov     byte [j_shl_c_alt+2],al
        GO_ON   j_shl_c_alt, OP_SHR_C_PRI, 8
    j_shl_c_alt:
        shl     edx,12h
	CHECKCODESIZE j_shl_c_alt

OP_SHR_C_PRI:
;nop;
        mov     al,[ebx+4]
        mov     byte [j_shr_c_pri+2],al
        GO_ON   j_shr_c_pri, OP_SHR_C_ALT, 8
    j_shr_c_pri:
        shr     eax,12h
	CHECKCODESIZE j_shr_c_pri

OP_SHR_C_ALT:
;nop;
        mov     al,[ebx+4]
        mov     byte [j_shr_c_alt+2],al
        GO_ON   j_shr_c_alt, OP_SMUL, 8
    j_shr_c_alt:
        shr     edx,12h
	CHECKCODESIZE j_shr_c_alt

OP_SMUL:
;nop;
        GO_ON   j_smul, OP_SDIV
    j_smul:
        push    edx
        imul    edx
        pop     edx
	CHECKCODESIZE j_smul

;good
OP_SDIV:
;nop;
        GO_ON   j_sdiv, OP_SDIV_ALT
    j_sdiv:
        call    [jit_sdiv]
	CHECKCODESIZE j_sdiv

OP_SDIV_ALT:
;nop;
        GO_ON   j_sdiv_alt, OP_UMUL
    j_sdiv_alt:
        xchg    eax,edx
        call    [jit_sdiv]
	CHECKCODESIZE j_sdiv_alt

OP_UMUL:
;nop;
        GO_ON   j_umul, OP_UDIV
    j_umul:
        push    edx
        mul     edx
        pop     edx
	CHECKCODESIZE j_umul

OP_UDIV:
;nop;
        GO_ON   j_udiv, OP_UDIV_ALT
    j_udiv:
        mov     ebp,edx
        sub     edx,edx
        call    [chk_dividezero]
        div     ebp
	CHECKCODESIZE j_udiv

OP_UDIV_ALT:
;nop;
        GO_ON   j_udiv_alt, OP_ADD
    j_udiv_alt:
        mov     ebp,eax
        mov     eax,edx
        sub     edx,edx
        call    [chk_dividezero]
        div     ebp
	CHECKCODESIZE j_udiv_alt

;good
OP_ADD:
;nop;
        GO_ON   j_add, OP_SUB
    j_add:
        add     eax,edx
	CHECKCODESIZE j_add

;good
OP_SUB:
;nop;
        GO_ON   j_sub, OP_SUB_ALT
    j_sub:
        sub     eax,edx
	CHECKCODESIZE j_sub

;good
OP_SUB_ALT:
;nop;
        GO_ON   j_sub_alt, OP_AND
    j_sub_alt:
        neg     eax
        add     eax,edx
	CHECKCODESIZE j_sub_alt

OP_AND:
;nop;
        GO_ON   j_and, OP_OR
    j_and:
        and     eax,edx
	CHECKCODESIZE j_and

OP_OR:
;nop;
        GO_ON   j_or, OP_XOR
    j_or:
        or      eax,edx
	CHECKCODESIZE j_or

OP_XOR:
;nop;
        GO_ON   j_xor, OP_NOT
    j_xor:
        xor     eax,edx
	CHECKCODESIZE j_xor

OP_NOT:
;nop;
        GO_ON   j_not, OP_NEG
    j_not:
        neg     eax             ; sets CF iff EAX != 0
        sbb     eax,eax         ; EAX == -1 iff CF set (zero otherwise)
        inc     eax             ; -1 => 0 and 0 => 1
	CHECKCODESIZE j_not

OP_NEG:
;nop;
        GO_ON   j_neg, OP_INVERT
    j_neg:
        neg     eax
	CHECKCODESIZE j_neg

OP_INVERT:
;nop;
        GO_ON   j_invert, OP_ADD_C
    j_invert:
        not     eax
	CHECKCODESIZE j_invert

;good
OP_ADD_C:
;nop;
        putval  j_add_c+1
        GO_ON   j_add_c, OP_SMUL_C, 8
    j_add_c:
        add     eax,12345678h
	CHECKCODESIZE j_add_c

;good
OP_SMUL_C:
;nop;
        putval  j_smul_c+3
        GO_ON   j_smul_c, OP_ZERO_PRI, 8
    j_smul_c:
        push    edx
        imul    eax,12345678h
        pop     edx
	CHECKCODESIZE j_smul_c

;good
OP_ZERO_PRI:
;nop;
        GO_ON   j_zero_pri, OP_ZERO_ALT
    j_zero_pri:
        sub     eax,eax
	CHECKCODESIZE j_zero_pri

;good
OP_ZERO_ALT:
;nop;
        GO_ON   j_zero_alt, OP_ZERO
    j_zero_alt:
        sub     edx,edx
	CHECKCODESIZE j_zero_alt

OP_ZERO:
;nop;
        putval  j_zero+2
        GO_ON   j_zero, OP_ZERO_S, 8
    j_zero:
        mov     dword [edi+12345678h],0
	CHECKCODESIZE j_zero

OP_ZERO_S:
;nop;
        putval  j_zero_s+2
        GO_ON   j_zero_s, OP_SIGN_PRI, 8
    j_zero_s:
        mov     dword [ebx+12345678h],0
	CHECKCODESIZE j_zero_s

OP_SIGN_PRI:
;nop;
        GO_ON   j_sign_pri, OP_SIGN_ALT
    j_sign_pri:
        shl     eax,24
        sar     eax,24
	CHECKCODESIZE j_sign_pri

OP_SIGN_ALT:
;nop;
        GO_ON   j_sign_alt, OP_EQ
    j_sign_alt:
        shl     edx,24
        sar     edx,24
	CHECKCODESIZE j_sign_alt

OP_EQ:
;nop;
        GO_ON   j_eq, OP_NEQ
    j_eq:
        cmp     eax,edx         ; PRI == ALT ?
        mov     eax,0
        sete    al
	CHECKCODESIZE j_eq

OP_NEQ:
;nop;
        GO_ON   j_neq, OP_LESS
    j_neq:
        cmp     eax,edx         ; PRI != ALT ?
        mov     eax,0
        setne   al
	CHECKCODESIZE j_neq

OP_LESS:
;nop;
        GO_ON   j_less, OP_LEQ
    j_less:
        cmp     eax,edx         ; PRI < ALT ? (unsigned)
        mov     eax,0
        setb    al
	CHECKCODESIZE j_less

OP_LEQ:
;nop;
        GO_ON   j_leq, OP_GRTR
    j_leq:
        cmp     eax,edx         ; PRI <= ALT ? (unsigned)
        mov     eax,0
        setbe   al
	CHECKCODESIZE j_leq

OP_GRTR:
;nop;
        GO_ON   j_grtr, OP_GEQ
    j_grtr:
        cmp     eax,edx         ; PRI > ALT ? (unsigned)
        mov     eax,0
        seta    al
	CHECKCODESIZE j_grtr

OP_GEQ:
;nop;
        GO_ON   j_geq, OP_SLESS
    j_geq:
        cmp     eax,edx         ; PRI >= ALT ? (unsigned)
        mov     eax,0
        setae   al
	CHECKCODESIZE j_geq

;good
OP_SLESS:
;nop;
        GO_ON   j_sless, OP_SLEQ
    j_sless:
        cmp     eax,edx         ; PRI < ALT ? (signed)
        mov     eax,0
        setl    al
	CHECKCODESIZE j_sless

OP_SLEQ:
;nop;
        GO_ON   j_sleq, OP_SGRTR
    j_sleq:
        cmp     eax,edx         ; PRI <= ALT ? (signed)
        mov     eax,0
        setle   al
	CHECKCODESIZE j_sleq

OP_SGRTR:
;nop;
        GO_ON   j_sgrtr, OP_SGEQ
    j_sgrtr:
        cmp     eax,edx         ; PRI > ALT ? (signed)
        mov     eax,0
        setg    al
	CHECKCODESIZE j_sgrtr

OP_SGEQ:
;nop;
        GO_ON   j_sgeq, OP_EQ_C_PRI
    j_sgeq:
        cmp     eax,edx         ; PRI >= ALT ? (signed)
        mov     eax,0
        setge   al
	CHECKCODESIZE j_sgeq

OP_EQ_C_PRI:
;nop;
        putval  j_eq_c_pri+1
        GO_ON   j_eq_c_pri, OP_EQ_C_ALT, 8
    j_eq_c_pri:
        cmp     eax,12345678h   ; PRI == value ?
        mov     eax,0
        sete    al
	CHECKCODESIZE j_eq_c_pri

OP_EQ_C_ALT:
;nop;
        putval  j_eq_c_alt+4
        GO_ON   j_eq_c_alt, OP_INC_PRI, 8
    j_eq_c_alt:
        sub     eax,eax
        cmp     edx,12345678h   ; ALT == value ?
        sete    al
	CHECKCODESIZE j_eq_c_alt

OP_INC_PRI:
;nop;
        GO_ON   j_inc_pri, OP_INC_ALT
    j_inc_pri:
        inc     eax
	CHECKCODESIZE j_inc_pri

OP_INC_ALT:
;nop;
        GO_ON   j_inc_alt, OP_INC
    j_inc_alt:
        inc     edx
	CHECKCODESIZE j_inc_alt

OP_INC:
;nop;
        putval  j_inc+2
        GO_ON   j_inc, OP_INC_S, 8
    j_inc:
        inc     dword [edi+12345678h]
	CHECKCODESIZE j_inc

;good
OP_INC_S:
;nop;
        putval  j_inc_s+2
        GO_ON   j_inc_s, OP_INC_I, 8
    j_inc_s:
        inc     dword [ebx+12345678h]
	CHECKCODESIZE j_inc_s

OP_INC_I:
;nop;
        GO_ON   j_inc_i, OP_DEC_PRI
    j_inc_i:
        inc     dword [edi+eax]
	CHECKCODESIZE j_inc_i

OP_DEC_PRI:
;nop;
        GO_ON   j_dec_pri, OP_DEC_ALT
    j_dec_pri:
        dec     eax
	CHECKCODESIZE j_dec_pri

OP_DEC_ALT:
;nop;
        GO_ON   j_dec_alt, OP_DEC
    j_dec_alt:
        dec     edx
	CHECKCODESIZE j_dec_alt

OP_DEC:
;nop;
        putval  j_dec+2
        GO_ON   j_dec, OP_DEC_S, 8
    j_dec:
        dec     dword [edi+12345678h]
	CHECKCODESIZE j_dec

OP_DEC_S:
;nop;
        putval  j_dec_s+2
        GO_ON   j_dec_s, OP_DEC_I, 8
    j_dec_s:
        dec     dword [ebx+12345678h]
	CHECKCODESIZE j_dec_s

OP_DEC_I:
;nop;
        GO_ON   j_dec_i, OP_MOVS
    j_dec_i:
        dec     dword [edi+eax]
	CHECKCODESIZE j_dec_i

OP_MOVS:
;nop;
        putval  j_movs+2
        GO_ON   j_movs, OP_CMPS, 8
    j_movs:
    	push	ecx
        mov     ecx,12345678h
        call    [jit_movs]
        pop		ecx
	CHECKCODESIZE j_movs

OP_CMPS:
;nop;
        putval  j_cmps+2
        GO_ON   j_cmps, OP_FILL, 8
    j_cmps:
        push	ecx
        mov     ecx,12345678h
        call    [jit_cmps]
        pop		ecx
	CHECKCODESIZE j_cmps

OP_FILL:
;nop;
        putval  j_fill+2
        GO_ON   j_fill, OP_HALT, 8
    j_fill:
        push	ecx
        mov     ecx,12345678h   ;TODO: save ECX if used as special register
        call    [jit_fill]
        pop		ecx
	CHECKCODESIZE j_fill

;good
OP_HALT:
;nop;
        putval  j_halt_sm+1
        GO_ON   j_halt, OP_BOUNDS, 8
    j_halt:
        cmp     dword retval,0
        je      j_halt_no_value
        mov     ebp,retval
        mov     [ebp],eax
    j_halt_no_value:
    j_halt_sm:
        mov     eax,12345678h
        jmp     [jit_return]
	CHECKCODESIZE j_halt

;good
OP_BOUNDS:
;nop;
        putval  j_bounds+1
        GO_ON   j_bounds, OP_SYSREQ_C, 8
    j_bounds:
        mov     ebp,12345678h
        call    [jit_bounds]
	CHECKCODESIZE j_bounds

;good
OP_SYSREQ_C:
;nop;
        putval  j_sysreq_c+1
        GO_ON   j_sysreq_c, OP_SYSREQ_PRI, 8
    j_sysreq_c:
        mov     eax,12345678h   ; get function number
    j_sysreq:
        call    [jit_sysreq]
	CHECKCODESIZE j_sysreq_c
        ; GWMV: oh well, it may look stupid, but I don't want to miss anything
        CHECKCODESIZE j_sysreq

OP_SYSREQ_PRI:
;nop;
        GO_ON   j_sysreq, OP_SYSREQ_PRI

OP_FILE:                                ;opcode is simply ignored
;nop;
        mov     eax,[ebx+4]             ;get size
        mov     [ebx],edi
        lea     ebx,[ebx+eax+8]         ;move on to next opcode
        cmp     ebx,dword [end_code]
        jae     code_gen_done
        jmp     dword [ebx]         ;go on with the next opcode

OP_LINE:
;nop;
        mov     [ebx],edi               ; no line number support: ignore opcode
        add     ebx,12                  ; move on to next opcode
        cmp     ebx,[end_code]
        jae     code_gen_done
        jmp     dword [ebx]         ; go on with the next opcode

OP_SYMBOL:                              ;ignored
        mov     [ebx],edi
        mov     eax,[ebx+4]             ; get size
        lea     ebx,[ebx+eax+8]         ; move on to next opcode
        cmp     ebx,[end_code]
        jae     code_gen_done
        jmp     dword [ebx]         ; go on with the next opcode


OP_SRANGE:                              ;ignored
        mov     [ebx],edi               ; store relocated address
        add     ebx,12                  ; move on to next opcode
        cmp     ebx,[end_code]
        jae     code_gen_done
        jmp     dword [ebx]         ; go on with the next opcode


;not tested
OP_JUMP_PRI:
        GO_ON   j_jump_pri, OP_SWITCH

    j_jump_pri:                 ; MP: This opcode makes sense only in con-
        jmp     [eax]           ; junction with a possibility to get the
                                ; address of a code location...
	CHECKCODESIZE j_jump_pri


;good
OP_SWITCH:
        lea     eax,[edi+6]     ; The case table will be copied directly
        neg     eax             ; after the run-time call to [jit_switch].
        and     eax,3           ; We should align this table on a DWORD
        mov     ecx,eax         ; boundary.
        mov     al,90h          ; 90h = opcode of x86 NOP instruction
        rep  stosb              ; Write the right number of NOPs.
        mov     [ebx],edi       ; store address of SWITCH for relocation step
        mov     esi, j_switch
        mov     ecx,6
        rep  movsb              ; copy the call instruction
        mov     esi,[ebx+4]     ; get address of CASETBL instruction
        add     ebx,8           ; set instruction pointer to next opcode
        add     esi,4           ; point esi to first entry: (count, default adr)
        mov     ecx,[esi]       ; get number of cases (excluding default)
        inc     ecx
        mov     ebp,[reloc_num]
    j_case_loop:
        mov     eax,[esi]       ; get case value
        stosd                   ; write it
        mov     eax,[esi+4]     ; get destination address
%ifndef FORCERELOCATABLE
        or      eax,80000000h   ; add flag for "absolute address"
%endif
        mov     [edx+ebp],eax   ; write dest. adr. into relocation table
        mov     eax,[esi+4]     ; get destination address (again)
        add     esi,8           ; set ESI to next case
        mov     [edx+ebp+4],edi ; write adr. to patch into relocation table
        add     ebp,8           ; promote relocation pointer
        stosd                   ; write dest. adr.
        dec     ecx
        jnz     j_case_loop
        mov     dword [reloc_num],ebp       ; write back updated reloc_num

        jmp     [ebx]           ; GO_ON to next op-code

    j_switch:
        call    [jit_switch]

;good
OP_CASETBL:                     ; compiles to nothing, SWITCH does all the work
        mov     eax,[ebx+4]     ; get count of cases
        lea     ebx,[ebx+8*eax+(8+4)]   ; adjust instruction pointer
        jmp     [ebx]           ; GO_ON with next op-code


OP_SWAP_PRI:                    ; TR
        GO_ON   j_swap_pri, OP_SWAP_ALT

        j_swap_pri:
        _POP    ebp
        _PUSH   eax
        mov     eax,ebp
	CHECKCODESIZE j_swap_pri


OP_SWAP_ALT:                    ; TR
        GO_ON   j_swap_alt, OP_PUSHADDR

        j_swap_alt:
        _POP    ebp
        _PUSH   edx
        mov     edx,ebp
	CHECKCODESIZE j_swap_alt


OP_PUSHADDR:                    ; TR
        putval  j_pushaddr+1
        GO_ON   j_pushaddr, OP_NOP, 8

    j_pushaddr:
        mov     ebp,12345678h   ;get address (offset from frame)
        add     ebp,frm
        _PUSH   ebp
	CHECKCODESIZE j_pushaddr


OP_NOP:                         ; TR
        GO_ON   j_nop, OP_SYSREQ_D
     j_nop:                     ; code alignment is ignored by the JIT
	CHECKCODESIZE j_nop


OP_SYSREQ_D:
;nop;
        putval  j_sysreq_d+1
        GO_ON   j_sysreq_d, OP_SYMTAG, 8
    j_sysreq_d:
        mov     ebx,12345678h   ; get function address
        call    [jit_sysreq_d]
	CHECKCODESIZE j_sysreq_d


OP_SYMTAG:                              ;ignored (TR)
        mov     [ebx],edi               ; store relocated address
        add     ebx,8                   ; move on to next opcode
        cmp     ebx,[end_code]
        jae     code_gen_done
        jmp     dword [ebx]         ; go on with the next opcode


OP_BREAK:
%ifndef DEBUGSUPPORT
        mov     [ebx],edi               ; no line number support: ignore opcode
        add     ebx,4                   ; move on to next opcode
        cmp     ebx,[end_code]
        jae     code_gen_done
        jmp     DWORD [ebx]             ; go on with the next opcode
%else
        GO_ON   j_break, OP_FLOAT_MUL
    j_break:
        mov     ebp,amx
        cmp     DWORD [ebp+_debug], 0
        je      $+4                     ; jump around the "call" statement
        call    [jit_break]
    CHECKCODESIZE j_break
%endif

OP_FLOAT_MUL:
		GO_ON	j_float_mul, OP_FLOAT_DIV
	j_float_mul:
		movss	xmm0, dword [esi+4]
		mulss	xmm0, dword [esi+8]
		movd	eax, xmm0
	CHECKCODESIZE j_float_mul
	
OP_FLOAT_DIV:
		GO_ON	j_float_div, OP_FLOAT_ADD
	j_float_div:
		movss	xmm0, dword [esi+4]
		divss	xmm0, dword [esi+8]
		movd	eax, xmm0
	CHECKCODESIZE j_float_div
	
OP_FLOAT_ADD:
		GO_ON	j_float_add, OP_FLOAT_SUB
	j_float_add:
		movss	xmm0, dword [esi+4]
		addss	xmm0, dword [esi+8]
		movd	eax, xmm0
	CHECKCODESIZE j_float_add
	
OP_FLOAT_SUB:
		GO_ON	j_float_sub, OP_FLOAT_TO
	j_float_sub:
		movss	xmm0, dword [esi+4]
		subss	xmm0, dword [esi+8]
		movd	eax, xmm0
	CHECKCODESIZE j_float_sub
	
OP_FLOAT_TO:
		GO_ON   j_float_to, OP_FLOAT_ROUND
	j_float_to:
		cvtsi2ss	xmm0, dword [esi+4]
		movd	eax, xmm0
	CHECKCODESIZE j_float_to
	
OP_FLOAT_ROUND:
		GO_ON   j_float_round, OP_FLOAT_CMP
	j_float_round:
		cmp dword [esi+8], 0
		jne .Floor
	;if (arg2 == 0)			ROUND
	;{
		cvtss2si	eax, dword [esi+4]
	;}
		jmp .Done
	.Floor:
		cmp dword [esi+8], 1
		jne .Ceil
	;else if (arg2 == 1)	FLOOR
	;{
		cvttss2si		eax, dword [esi+4]
		mov				ebp, dword [esi+4]
		shr				ebp, 31
		sub				eax, ebp
	;}
		jmp .Done
	.Ceil:
		cmp dword [esi+8], 2
		jne .Zero
	;else if (arg2 == 2)	CEIL
	;{
		movss		xmm0, dword [esi+4]
		addss		xmm0, dword [g_round_nearest]
		cvtss2si	eax, xmm0
	;}
		jmp .Done
	.Zero:
	;else					ZERO
	;{
		cvttss2si	eax, dword [esi+4]
	;}
	.Done:
	CHECKCODESIZE j_float_round
	
OP_FLOAT_CMP:
		GO_ON	j_float_cmp, OP_INVALID
	j_float_cmp:
		movss	xmm0, dword [esi+8]
		movss	xmm1, dword [esi+4]
		ucomiss	xmm1, xmm0
		cmovz   eax, [g_flagsjit+4]
		cmova   eax, [g_flagsjit+8]
		cmovb   eax, [g_flagsjit+0]
	CHECKCODESIZE j_float_cmp

OP_INVALID:                     ; break from the compiler with an error code
        mov     eax,AMX_ERR_INVINSTR
        pop     esi
        pop     edi
        pop     ecx
        pop     ebp
        ret


section .text

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                               ;
;cell   amx_exec( cell *regs, cell *retval, cell stp, cell hea );
;                       eax         edx          ebx       ecx  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

amx_exec_jit:
_amx_exec_jit:
        push    edi
        push    esi
        push    ebp
        push    ebx             ; due to __cdecl

        ; __cdecl overhead
        mov     eax, [esp+20]   ; get address of amx regs structure
        mov     edx, [esp+24]   ; get address of retval
        mov     ebx, [esp+28]   ; get stp
        mov     ecx, [esp+32]   ; get hea

        sub     esp,4*3         ; place for PRI, ALT & STK at SYSREQs

        push    dword [eax+28] ; store pointer to code segment
        push    dword [eax+24] ; store pointer to AMX
        push    edx             ; store address of retval
        push    ebx             ; store STP
        push    ecx             ; store HEA
        push    dword [eax+20]; store FRM

        mov     edx,[eax+4]     ; get ALT
        mov     ebp,[eax+8]     ; get CIP
        mov     edi,[eax+12]    ; get pointer to data segment
        mov     esi,[eax+16]    ; get STK !!changed, now ECX free as counter!!
        mov     ebx,[eax+20]    ; get FRM
        mov     eax,[eax]       ; get PRI
        add     ebx,edi         ; relocate frame

        add     esi,edi         ; ESP will contain DAT+STK

        add     [esp+8],edi     ; make STP absolute address for run-time checks

        mov		dword [esi], 0	; zero this out, but we need to keep it so
        						; the stack frame is in tact
        mov		ecx,esp			; copy stack pointer
        ; Call compiled code via CALL NEAR <address>
        call    ebp

return_to_caller:
        cmp     dword retval,0
        je      return_to_caller_no_value
        mov     ebp,retval
        mov     [ebp],eax       ; provide return value

    return_to_caller_no_value:
        mov     eax,AMX_ERR_NONE
        jmp     _return

_return_popstack:
		mov		esp,ecx			; get our old stack pointer
_return:
        ; store machine state
        push	ecx
        push	ecx
        mov     ebp,amx         ; get amx into EBP
        mov     ecx,esi         ; get STK into ECX

        sub     ecx,edi         ; correct STK
        mov     [ebp+_stk],ecx  ; store values in AMX structure: STK, ...
        pop		ecx				; get orig value
        mov     ecx,hea         ; ... HEA, ...
        mov     [ebp+_hea],ecx
        mov     ecx,ebx         ; ... and FRM
        sub     ecx,edi         ; (correct FRM)
        mov     [ebp+_frm],ecx
        mov     [ebp+_pri],eax  ; also store PRI, ...
        mov     [ebp+_alt],edx  ; ... and ALT

        ; return
        pop		ecx
        sub     stp,edi         ; make STP relative to DAT again

        add     esp,4*9         ; remove temporary data

        pop     ebx             ; restore registers that have to be preserved
        pop     ebp             ; when using __cdecl convention
        pop     esi
        pop     edi

        ret


err_stack:
        mov     eax,AMX_ERR_STACKERR
        jmp     _return_popstack

err_stacklow:
        mov     eax,AMX_ERR_STACKLOW
        jmp     _return_popstack

_CHKMARGIN_STACK:               ; some run-time check routines
        cmp     esi,stp
        jg      err_stacklow
        ret

err_heaplow:
        mov     eax,AMX_ERR_HEAPLOW
        jmp     _return_popstack

_CHKMARGIN_HEAP:
        cmp     esi,stp
        jg      err_stacklow
        mov     ebp,amx
        mov     ebp,[ebp+_hlw]
        cmp     dword hea,ebp
        jl      err_heaplow
        ret

err_memaccess:
        mov     eax,AMX_ERR_MEMACCESS
        jmp     _return_popstack

_VERIFYADDRESS_eax:             ; used in load.i, store.i & lidx
        cmp     eax,stp
        jae     err_memaccess
        cmp     eax,hea
        jb      veax1
        lea     ebp,[eax+edi]
        cmp     ebp,esi
        jb      err_memaccess
    veax1:
        ret

_VERIFYADDRESS_edx:             ; used in load.i, store.i & lidx
        cmp     edx,stp
        jae     err_memaccess
        cmp     edx,hea
        jb      vedx1
        lea     ebp,[edx+edi]
        cmp     ebp,esi
        jb      err_memaccess
    vedx1:
        ret

JIT_OP_SDIV:
        mov     ebp,edx
        xor     edx,eax         ; Check signs of the operands.
        cdq
        js      sdiv_fiddle     ; If the signs of the operands are different
                                ; we'll have to fiddle around to achieve
                                ; proper rounding towards minus infinity.
        or      ebp,ebp         ; check for divide by zero
        jz      err_divide
        idiv    ebp             ; default behavior is right in the other cases
        ret

    sdiv_fiddle:
        or      ebp,ebp         ; check for divide by zero
        jz      err_divide
        idiv    ebp
        or      edx,edx
        jz      sdiv_goon       ; If there's no remainder the result is correct
        add     edx,ebp         ; else fix the result values.
        dec     eax             ; Amazing, how simple this is...
    sdiv_goon:
        ret

        ALIGN   4

JIT_OP_RETN:
        _POP    ebx             ; pop frame
        add		esi,4			; get rid of the extra parameter from call

        mov     frm,ebx
        _POP    ebp

        add     ebx,edi
        add     esi,ebp         ; remove data from stack

        ret


JIT_OP_MOVS:                    ;length of block to copy is already in ECX
        push    edi
        push    esi
        lea     esi,[edi+eax]
        lea     edi,[edi+edx]

        push    ecx             ; I hope the blocks to copy are properly
        shr     ecx,2           ; aligned, so I don't do anything about that.
        rep movsd
        pop     ecx
        and     ecx,3
        rep movsb

        pop     esi
        pop     edi
        ret

JIT_OP_CMPS:                    ;length of block to compare is already in ECX
        push    edi
        push    esi
        lea     esi,[edi+edx]
        lea     edi,[edi+eax]

        xor     eax,eax         ; This is surely not the fastest way to do this
        repe cmpsb              ; but the most simple one.
        je      cmps1
        sbb     eax,eax
        sbb     eax,0ffffffffh
    cmps1:
        pop     esi
        pop     edi
        ret


JIT_OP_FILL:                    ;length (in bytes) of block to fill is already in ECX
        push    edi
        lea     edi,[edi+edx]

        shr     ecx,2           ;length in 32-bit cells
        rep stosd               ;the value to use is already in EAX

        pop     edi
        ret

JIT_OP_BOUNDS:
        cmp     eax,0
        jl      err_bounds
        cmp     eax,ebp
        jg      err_bounds
        ret
err_bounds:
        mov     eax,AMX_ERR_BOUNDS
        jmp     _return_popstack

_CHKDIVIDEZERO:
        or      ebp,ebp         ; check for divide by zero
        jz      err_divide
        ret
err_divide:
        mov     eax,AMX_ERR_DIVIDE
        jmp     _return_popstack

JIT_OP_SYSREQ:
        _STK_ALIGN 32           ; align stack to 16-byte boundary and
                                ; allocate 32 bytes of stack space
        mov     [esp+16], ecx
        mov     [esp+12], esi
        mov     ebp,amx         ; get amx into EBP

        sub     esi,edi         ; correct STK
        mov     alt,edx         ; save ALT

        mov     [ebp+_stk],esi  ; store values in AMX structure: STK,
        mov     esi,hea         ; HEA,
        mov     ebx,frm         ; and FRM
        mov     [ebp+_hea],esi
        mov     [ebp+_frm],ebx

        lea     ebx,pri         ; 3rd param: addr. of retval

        ;Our original esi is still pushed!
        mov     [esp+08], ebx
        mov     [esp+04], eax   ; 2nd param: function number
        mov     [esp], ebp      ; 1st param: amx
        call    [ebp+_callback]
        
        mov     esi, [esp+12]   ; restore esi
        mov     ecx, [esp+16]   ; restore ecx
        _STK_RESTORE            ; restore stack pointer

        cmp     eax,AMX_ERR_NONE
        jne		_return_popstack
.continue:
        mov     eax,pri         ; get retval into eax (PRI)
        mov     edx,alt         ; restore ALT
        mov     ebx,frm         ; restore FRM
        add     ebx,edi         ; relocate frame
        ret


JIT_OP_SYSREQ_D:                ; (TR)
        _STK_ALIGN 16           ; align stack to 16-byte boundary and
                                ; allocate 16 bytes of stack space
        mov     [esp+08], ecx
        mov     [esp+04], esi
        mov     ebp,amx         ; get amx into EBP

        sub     esi,edi         ; correct STK
        mov     alt,edx         ; save ALT

        mov     [ebp+_stk],esi  ; store values in AMX structure: STK,
        mov     esi,hea         ; HEA,
        mov     eax,frm         ; and FRM
        mov     [ebp+_hea],esi
        mov     [ebp+_frm],eax  ; eax & ecx are invalid by now

        ;esi is still pushed!
        mov     [esp], ebp      ; 1st param: amx
        call    ebx             ; direct call

        mov     ecx, [esp+08]   ; restore ecx
        _STK_RESTORE            ; restore stack pointer

        mov     ebp,amx         ; get amx into EBP
        cmp     dword [ebp+_error],AMX_ERR_NONE
        jne     _return_popstack; return error code, if any

        ; return value is in eax (PRI)
        mov     edx,alt         ; restore ALT
        mov     ebx,frm         ; restore FRM
        add     ebx,edi         ; relocate frame
        ret


JIT_OP_BREAK:
%ifdef DEBUGSUPPORT
        _STK_ALIGN 16           ; align stack to 16-byte boundary and
                                ; allocate 16 bytes of stack space
        mov     [esp+08], ecx
        mov     [esp+04], esi
        mov     ebp,amx         ; get amx into EBP

        sub     esi,edi         ; correct STK

        mov     [ebp+_pri],eax  ; store values in AMX structure: PRI,
        mov     [ebp+_alt],edx  ; ALT,
        mov     [ebp+_stk],esi  ; STK,
        mov     esi,hea         ; HEA,
        mov     ebx,frm         ; and FRM
        mov     [ebp+_hea],esi
        mov     [ebp+_frm],ebx  ; EBX & ECX are invalid by now
        ;??? storing CIP is not very useful, because the code changed (during JIT compile)

        mov     [esp], ebp      ; 1st param: amx
        call    [ebp+_debug]

        mov esi, [esp+04]       ; restore esi
        mov ecx, [esp+08]       ; restore ecx
        _STK_RESTORE            ; restore stack pointer

        cmp     eax,AMX_ERR_NONE
        jne     _return_popstack; return error code, if any

        mov     ebp,amx         ; get amx into EBP
        mov     eax,[ebp+_pri]  ; restore values
        mov     edx,[ebp+_alt]  ; ALT,
        mov     edx,alt         ; restore ALT
        mov     ebx,frm         ; restore FRM
        add     ebx,edi         ; relocate frame
%endif
        ret


JIT_OP_SWITCH:
        pop     ebp             ; pop return address = table address
        push	ecx
        mov     ecx,[ebp]       ; ECX = number of records
        lea     ebp,[ebp+ecx*8+8]       ; set pointer _after_ LAST case
        ;if there are zero cases we should just skip this -- bail
        test	ecx, ecx
        jz		op_switch_jump
    op_switch_loop:
        cmp     eax,[ebp-8]     ; PRI == case label?
        je      op_switch_jump  ; found, jump
        sub     ebp,8           ; position to preceding case
        loop    op_switch_loop  ; check next case, or fall through
    op_switch_jump:
    	pop		ecx
%ifndef FORCERELOCATABLE
        jmp     [ebp-4]         ; jump to the case instructions
%else
        add     ebp,[ebp-4]     ; add offset to make absolute adddress
        jmp     ebp
%endif



; The caller of asm_runJIT() can determine the maximum size of the compiled
; code by multiplying the result of this function by the number of opcodes in
; Pawn module.
;
; unsigned long getMaxCodeSize_();
;
getMaxCodeSize:
_getMaxCodeSize:
        mov     eax,MAXCODESIZE
        ret

section .data
        ALIGN   4       ; This is essential to avoid misalignment stalls.

end_code        DD  0   ; pointer to the end of the source code

compiled_code   DD  0   ; pointer to compiled code (including preamble)

amxhead         DD  0   ; pointer to the AMX_HEADER struct (arg #1 to runJIT)

reloc_num       DD  0   ; counts the addresses in the relocation table (jumps)

lodb_and        DD  0ffh, 0ffffh, 0, 0ffffffffh

;
; A list of the "run-time-library" functions that are called via indirect calls.
; So these calls don't have to be relocated. This gives also the possibility to
; replace some of these with shorter/faster non-debug or non-checking versions,
; without changing the compiled code. Instead this table could be changed...
;
verify_adr_eax  DD      _VERIFYADDRESS_eax
verify_adr_edx  DD      _VERIFYADDRESS_edx
chk_marginstack DD      _CHKMARGIN_STACK
chk_marginheap  DD      _CHKMARGIN_HEAP
chk_dividezero  DD      _CHKDIVIDEZERO
jit_return      DD      _return
jit_retn        DD      JIT_OP_RETN
jit_sdiv        DD      JIT_OP_SDIV
jit_movs        DD      JIT_OP_MOVS
jit_cmps        DD      JIT_OP_CMPS
jit_fill        DD      JIT_OP_FILL
jit_bounds      DD      JIT_OP_BOUNDS
jit_sysreq      DD      JIT_OP_SYSREQ
jit_sysreq_d    DD      JIT_OP_SYSREQ_D
jit_break       DD      JIT_OP_BREAK
jit_switch      DD      JIT_OP_SWITCH

;
; The table for the browser/relocator function.
;

global g_flagsjit
g_flagsjit:
		DD		-1
		DD		0
		DD		1
		
global g_round_nearest
g_round_nearest:
		DD		0.5

global amx_opcodelist_jit, _amx_opcodelist_jit

amx_opcodelist_jit:
_amx_opcodelist_jit:
        DD      OP_INVALID
        DD      OP_LOAD_PRI
        DD      OP_LOAD_ALT
        DD      OP_LOAD_S_PRI
        DD      OP_LOAD_S_ALT
        DD      OP_LREF_PRI
        DD      OP_LREF_ALT
        DD      OP_LREF_S_PRI
        DD      OP_LREF_S_ALT
        DD      OP_LOAD_I
        DD      OP_LODB_I
        DD      OP_CONST_PRI
        DD      OP_CONST_ALT
        DD      OP_ADDR_PRI
        DD      OP_ADDR_ALT
        DD      OP_STOR_PRI
        DD      OP_STOR_ALT
        DD      OP_STOR_S_PRI
        DD      OP_STOR_S_ALT
        DD      OP_SREF_PRI
        DD      OP_SREF_ALT
        DD      OP_SREF_S_PRI
        DD      OP_SREF_S_ALT
        DD      OP_STOR_I
        DD      OP_STRB_I
        DD      OP_LIDX
        DD      OP_LIDX_B
        DD      OP_IDXADDR
        DD      OP_IDXADDR_B
        DD      OP_ALIGN_PRI
        DD      OP_ALIGN_ALT
        DD      OP_LCTRL
        DD      OP_SCTRL
        DD      OP_MOVE_PRI
        DD      OP_MOVE_ALT
        DD      OP_XCHG
        DD      OP_PUSH_PRI
        DD      OP_PUSH_ALT
        DD      OP_PUSH_R_PRI
        DD      OP_PUSH_C
        DD      OP_PUSH
        DD      OP_PUSH_S
        DD      OP_POP_PRI
        DD      OP_POP_ALT
        DD      OP_STACK
        DD      OP_HEAP
        DD      OP_PROC
        DD      OP_RET
        DD      OP_RETN
        DD      OP_CALL
        DD      OP_CALL_I
        DD      OP_JUMP
        DD      OP_JREL
        DD      OP_JZER
        DD      OP_JNZ
        DD      OP_JEQ
        DD      OP_JNEQ
        DD      OP_JLESS
        DD      OP_JLEQ
        DD      OP_JGRTR
        DD      OP_JGEQ
        DD      OP_JSLESS
        DD      OP_JSLEQ
        DD      OP_JSGRTR
        DD      OP_JSGEQ
        DD      OP_SHL
        DD      OP_SHR
        DD      OP_SSHR
        DD      OP_SHL_C_PRI
        DD      OP_SHL_C_ALT
        DD      OP_SHR_C_PRI
        DD      OP_SHR_C_ALT
        DD      OP_SMUL
        DD      OP_SDIV
        DD      OP_SDIV_ALT
        DD      OP_UMUL
        DD      OP_UDIV
        DD      OP_UDIV_ALT
        DD      OP_ADD
        DD      OP_SUB
        DD      OP_SUB_ALT
        DD      OP_AND
        DD      OP_OR
        DD      OP_XOR
        DD      OP_NOT
        DD      OP_NEG
        DD      OP_INVERT
        DD      OP_ADD_C
        DD      OP_SMUL_C
        DD      OP_ZERO_PRI
        DD      OP_ZERO_ALT
        DD      OP_ZERO
        DD      OP_ZERO_S
        DD      OP_SIGN_PRI
        DD      OP_SIGN_ALT
        DD      OP_EQ
        DD      OP_NEQ
        DD      OP_LESS
        DD      OP_LEQ
        DD      OP_GRTR
        DD      OP_GEQ
        DD      OP_SLESS
        DD      OP_SLEQ
        DD      OP_SGRTR
        DD      OP_SGEQ
        DD      OP_EQ_C_PRI
        DD      OP_EQ_C_ALT
        DD      OP_INC_PRI
        DD      OP_INC_ALT
        DD      OP_INC
        DD      OP_INC_S
        DD      OP_INC_I
        DD      OP_DEC_PRI
        DD      OP_DEC_ALT
        DD      OP_DEC
        DD      OP_DEC_S
        DD      OP_DEC_I
        DD      OP_MOVS
        DD      OP_CMPS
        DD      OP_FILL
        DD      OP_HALT
        DD      OP_BOUNDS
        DD      OP_SYSREQ_PRI
        DD      OP_SYSREQ_C
        DD      OP_FILE
        DD      OP_LINE
        DD      OP_SYMBOL
        DD      OP_SRANGE
        DD      OP_JUMP_PRI
        DD      OP_SWITCH
        DD      OP_CASETBL
        DD      OP_SWAP_PRI     ; TR
        DD      OP_SWAP_ALT     ; TR
        DD      OP_PUSHADDR     ; TR
        DD      OP_NOP          ; TR
        DD      OP_SYSREQ_D     ; TR
        DD      OP_SYMTAG       ; TR
        DD      OP_BREAK        ; TR
        DD      OP_FLOAT_MUL	; DA
        DD      OP_FLOAT_DIV	; DA
        DD      OP_FLOAT_ADD	; DA
        DD      OP_FLOAT_SUB	; DA
        DD      OP_FLOAT_TO		; DA
        DD      OP_FLOAT_ROUND	; DA
        DD      OP_FLOAT_CMP	; DA

END:
