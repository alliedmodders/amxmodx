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
	get_user_name
	register_plugin
	register_concmd
	server_print

.DATA
Plugin	db	PLUGIN
Version	db	VERSION
Author	db	AUTHOR
Cmd	db	"amx_asmtest"
Callback db	"cmdCallback"
Descr db "Test"

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
	retn					;return + cleanup
ENDP

.DATA
HELLO	db	"Hello, %s!"

.CODE
PROC cmdCallback
	stack -128*CELL		;new memory
	zero.pri		;zero out pri
	addr.alt -128		;alt points to new variable
	fill -128*CELL		;zero out new variable
	push.c 127		;push bytecount arg to get_user_name
	push.alt		;push the value of alt (which is name)
	push.s	ARG(1)		;push the first argument onto the stack
	push.c 3*CELL		;push 3 arguments
	sysreq.c get_user_name	;call get_user_name();
	stack 4*CELL		;clean up stack
	pushaddr -128		;push the name
	push.c HELLO		;push the message
	push.c 2*CELL		;push 2 arguments
	sysreq.c server_print	;call server_print
	stack 3*CELL		;clean up the stack
	stack 128*CELL		;clean up the name variable
	zero.pri		;zero out pri
	retn
ENDP

.PUBLIC
	cmdCallback
	plugin_init