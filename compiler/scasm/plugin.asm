;(C)2004 David "BAILOPAN" Anderson
; Demonstration of AMX Mod X plugin writing in assembly.
#define VERSION "1.00"
#define AUTHOR "BAILOPAN"
#define PLUGIN "Asm Test"
#define CELL 		4
#macro	ARGN(argc)	(12+(argc*CELL))

.CODE
	halt	0	;Return point for end

.NATIVE
	get_user_name	;id, buffer[], maxLen
	register_plugin	;name[], version[], author[]
	register_concmd	;cmd[], callback[], access, descr[]
	server_print	;fmt[], ...

.DATA
Plugin	db	PLUGIN
Version	db	VERSION
Author	db	AUTHOR
Cmd	db	"amx_asmtest"
Callback db	"cmdCallback"
Descr db "Test"

.DATA
HELLO	db	"Hello, %s!"

.CODE
PROC cmdCallback
	stack -128*CELL
	zero.pri
	addr.alt -128*CELL
	fill 128*CELL
	push.c 127
	pushaddr -128*CELL
	push.s ARG(0)
	push.c CELL*3
	sysreq.c get_user_name
	stack CELL*4
	pushaddr -128*CELL
	push.c HELLO
	push.c CELL*2
	sysreq.c server_print
	stack CELL*3
	stack 128*CELL
	zero.pri
	retn
ENDP

.CODE
;Technically PROC could simply be "proc"
; this is more for reasons of readability.
; feel free to use "proc" and omit ENDP
; if you would like to code one huge list of instructions.
PROC plugin_init
	push.c Author			;push the plugin name
	push.c Version			;push the plugin version
	push.c Plugin			;push the plugin author
	push.c CELL*3			;push 3 arguments
	sysreq.c register_plugin	;call register_plugin
	stack CELL*4			;clean up the stack
	push.c Callback			;push string
	push.c CELL				;push one argument
	sysreq.c server_print	;call server_print
	stack CELL*2			;clean up the stack
	push.c Descr			;push the description
	push.c 0				;push the access level
	push.c Callback			;push callback
	push.c Cmd				;push the command
	push.c CELL*4			;push 4 arguments
	sysreq.c register_concmd	;call register_concmd
	stack CELL*5			;cleanup
	zero.pri				;zero pri
	retn					;return + cleanup
ENDP

.PUBLIC
	plugin_init
	cmdCallback