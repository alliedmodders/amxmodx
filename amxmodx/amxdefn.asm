; Definition of the AMX structure for assembler syntax (NASM)

	struc amx_s
_base:       resd 1
_dataseg:    resd 1
_callback:   resd 1
_debug:      resd 1
_cip:        resd 1
_frm:        resd 1
_hea:        resd 1
_hlw:        resd 1
_stk:        resd 1
_stp:        resd 1
_flags:      resd 1
_usertags:   resd 4	; 4 = AMX_USERNUM (#define'd in amx.h)
_userdata:   resd 4	; 4 = AMX_USERNUM (#define'd in amx.h)
_error:      resd 1
_paramcount: resd 1
_pri:        resd 1
_alt:        resd 1
_reset_stk:  resd 1
_reset_hea:  resd 1
_syscall_d:  resd 1
%ifdef JIT
        ; the two fields below are for the JIT; they do not exist in
        ; the non-JIT version of the abstract machine
_reloc_size: resd 1          ; memory block for relocations
_code_size:  resd 1          ; memory size of the native code
%endif
	endstruc

	struc amxhead_s
_size:       resd 1  ; size of the "file"
_magic:      resw 1  ; signature
_file_version: resb 1; file format version
_amx_version: resb 1 ; required version of the AMX
_h_flags:    resw 1
_defsize:    resw 1  ; size of one public/native function entry
_cod:        resd 1  ; initial value of COD - code block
_dat:        resd 1  ; initial value of DAT - data block
_h_hea:      resd 1  ; initial value of HEA - start of the heap
_h_stp:      resd 1  ; initial value of STP - stack top
_h_cip:      resd 1  ; initial value of CIP - the instruction pointer
_publics:    resd 1  ; offset to the "public functions" table
_natives:    resd 1  ; offset to the "native functions" table
_libraries:  resd 1  ; offset to the "library" table
_pubvars:    resd 1  ; offset to the "public variables" table
_tags:       resd 1  ; offset to the "public tagnames" table
_nametable:  resd 1  ; offset to the name table, file version 7 only
	endstruc


AMX_ERR_NONE        EQU 0
AMX_ERR_EXIT        EQU 1
AMX_ERR_ASSERT      EQU 2
AMX_ERR_STACKERR    EQU 3
AMX_ERR_BOUNDS      EQU 4
AMX_ERR_MEMACCESS   EQU 5
AMX_ERR_INVINSTR    EQU 6
AMX_ERR_STACKLOW    EQU 7
AMX_ERR_HEAPLOW     EQU 8
AMX_ERR_CALLBACK    EQU 9
AMX_ERR_NATIVE      EQU 10
AMX_ERR_DIVIDE      EQU 11      ; for catching divide errors
AMX_ERR_SLEEP       EQU 12

AMX_ERR_MEMORY      EQU 16
AMX_ERR_FORMAT      EQU 17
AMX_ERR_VERSION     EQU 18
AMX_ERR_NOTFOUND    EQU 19
AMX_ERR_INDEX       EQU 20
AMX_ERR_DEBUG       EQU 21
AMX_ERR_INIT        EQU 22
AMX_ERR_USERDATA    EQU 23
AMX_ERR_INIT_JIT    EQU 24
AMX_ERR_PARAMS      EQU 25
AMX_ERR_DOMAIN      EQU 26
AMX_ERR_GENERAL     EQU 27

AMX_FLAG_DEBUG      EQU 0002h   ; symbolic info. available
AMX_FLAG_COMPACT    EQU 0004h
AMX_FLAG_BYTEOPC    EQU 0008h
AMX_FLAG_NOCHECKS   EQU 0010h
AMX_FLAG_BROWSE     EQU 4000h
AMX_FLAG_RELOC      EQU 8000h   ; jump/call addresses relocated

