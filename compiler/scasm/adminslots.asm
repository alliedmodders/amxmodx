;(C)2004 David "BAILOPAN" Anderson
; Assembler version of Anti-Flood
#define VERSION 	"0.20-A"
#define PLUGIN_HANDLED
#macro	ARGN(argc)	(12+(argc*CELL))

.CODE
	halt	0	;Return point for end

.NATIVE
	register_plugin
	register_cvar
	random_num
	register_clcmd
	server_cmd
	get_maxplayers
	get_playersnum
	get_cvar_num
	set_cvar_num
	get_user_flags
	client_cmd
	get_user_userid
	
.DATA
	Plugin		db	"Slots Reservation"
	Version		db	VERSION
	Author		db	"AMXX Dev Team"
	Cvar		db	"amx_reservation"
	CvarVal		db	"1"
	Callback	db	"ackSignal"
	loopBack	db	"amxres0000"
	
.CODE
PROC plugin_init
	push.c 		Author			;push author
	push.c		Version			;push version
	push.c		Plugin			;push name
	push.c		CELL*3			;push 3 args
	sysreq.c register_plugin	;call register_plugin
	stack 		CELL*4			;clean-up
	
	push.c		CvarVal			;push Cvar initial value
	push.c		Cvar			;push Cvar
	push.c		CELL*2			;push 2 args
	sysreq.c register_cvar		;call register_cvar
	stack 		CELL*3			;clean-up
	
	push.c 		90				;push max range
	push.c		65				;push min range
	push.c		CELL*2			;push 2 args
	sysreq.c random_num			;call random_num
	stack 		CELL*3			;clean-up
	stor.pri	loopBack+CELL*6	;store pri in loopBack[6]
	
	push.c 		90				;push max range
	push.c		65				;push min range
	push.c		CELL*2			;push 2 args
	sysreq.c 	random_num		;call random_num
	stack 		CELL*3			;clean-up
	stor.pri	loopBack+CELL*7	;store pri in loopBack[7]
	
	push.c 		90				;push max range
	push.c		65				;push min range
	push.c		CELL*2			;push 2 args
	sysreq.c 	random_num		;call random_num
	stack 		CELL*3			;clean-up
	stor.pri	loopBack+CELL*8	;store pri in loopBack[8]
	
	push.c 		90				;push max range
	push.c		65				;push min range
	push.c		CELL*2			;push 2 args
	sysreq.c 	random_num		;call random_num
	stack 		CELL*3			;clean-up
	stor.pri	loopBack+CELL*9	;store pri in loopBack[9]
	
	push.c		Callback		;push callback
	push.c		loopBack		;push loopback
	push.c		CELL*2
	sysreq.c 	register_clcmd	;call register_clcmd
	stack 		CELL*3			;clean-up
	
	zero.pri					;return 0
	retn						;final cleanup
ENDP

.DATA
	KickMsg		db	"kick #%d \"Dropped due to slot reservation\""

.CODE
PROC ackSignal
	push.s		ARGN(0)			;push the first argument
	push.c		CELL*1			;push one argument
	sysreq.c 	get_user_userid	;call get_user_userid
	stack 		CELL*2			;clean-up
	
	push.pri					;push the result of the last call
	push.c 		KickMsg			;push the kick message
	push.c		CELL*2			;push 2 args
	sysreq.c 	server_cmd		;call server_cmd
	stack		CELL*3			;clean-up
	
	zero.pri					;return 0
	retn						;final cleanup
ENDP

.DATA
	VisCvar		db	"sv_visiblemaxplayers"
	
.CODE
;players, maxplayrs, limit
PROC setVisibleSlots
	stack 		-CELL			;new variable
	
	load.s.pri	ARGN(0)			;get first parameter into [PRI]
	add.c		1				;[PRI] += 1
	stor.s.pri	-CELL			;[var] = [PRI]
	
	load.s.pri	ARGN(0)			;Reset [PRI] to first param
	load.s.alt	ARGN(1)			;get second parameter into [ALT]
	jeq			setMax			;does players == maxplayers?
	jump		setLimitCheck	; -- no, jump to next check
setMax:							; -- yes
	stor.s.alt	-CELL			; store the maxplayers into [var]
	jump		endSetIf		; we're done, jump to the end
setLimitCheck:
	load.s.alt	ARGN(2)			;load the third param into [ALT]
	jless		setLimit		;is playres < limit?
	jump 		endSetIf		; -- no, jump to end
setLimit:						; -- yes
	stor.s.alt	-CELL			;store limit into [var]
endSetIf:
	
	push.s		-CELL			;push [var] onto stack
	push.c		VisCvar			;push cvar onto stack
	push.c		CELL*2			;push 2 args
	sysreq.c 	set_cvar_num	;call set_cvar_num
	stack 		CELL*3			;clean-up
	
	stack 		CELL			;restore stack
	zero.pri					;return 0
	retn						;final cleanup
ENDP

PROC client_authorized
	push.c		0				;push 0 args
	sysreq.c 	get_maxplayers	;call get_maxplayers
	stack		CELL			;clean-up
	push.pri					;store the result - maxplayers
	
	push.c		1				;push 1
	push.c		CELL			;push 1 args
	sysreq.c 	get_playersnum	;call get_playersnum
	stack		CELL*2			;clean-up
	push.pri					;store the result - players
	
	push.c		Cvar			;push cvar
	push.c		CELL			;push 1 arg
	sysreq.c 	get_cvar_num		;call get_cvar_num
	stack		CELL*2			;clean-up
	xchg						;last result is now in alt
	
	load.s.pri	-CELL			;load the first result into pri
	sub							;pri = maxplayers - cvar
	push.pri					;store the result - limit
	
	push.s		ARGN(0)			;push the id
	push.c		CELL			;push 1 arg
	sysreq.c 	get_user_flags	;call get_user_flags
	stack 		CELL*2			;clean-up
	const.alt	2				;2 = ADMIN_RESERVATION
	and							;flags & 2
	
	jeq			setVis			;if (flags & 2) == 2, short circuit
								;otherwise check this condition
	load.s.pri	-CELL*2			;load players into pri
	load.s.alt	-CELL*3			;load limit into alt
	jleq		setVis			;if players <= limit, jump
	jump		setVisSkip		;otherwise skip
setVis:
	push.s		-CELL*3			;push limit
	push.s		-CELL			;push maxplayers
	push.s		-CELL*2			;push players
	push.c		CELL*3			;push 3 args
	call		setVisibleSlots	;call setVisibleSlots
	stack		CELL*3			;restore stack
	zero.pri					;return 0
	retn						;finalize
setVisSkip:
	
	push.c		loopBack		;push loopback cmd
	push.s		ARGN(0)			;push the id passed
	push.c		CELL*2			;push 2 args
	sysreq.c 	client_cmd		;call client_cmd
	stack		CELL*3			;clean-up
	
	stack		CELL*3			;restore stack
	const.pri	PLUGIN_HANDLED	;return
	retn						;finalize	
ENDP

PROC client_disconnect
	push.c		0				;push 0 args
	sysreq.c 	get_maxplayers	;call get_maxplayers
	stack		CELL			;clean-up
	push.pri					;store the result - maxplayers
	
	push.c		1				;push 1
	push.c		CELL			;push 1 args
	sysreq.c 	get_playersnum	;call get_playersnum
	stack		CELL*2			;clean-up
	push.pri					;store the result - players
	dec.s		-CELL*2			;players--
	
	push.c		Cvar			;push cvar
	push.c		CELL			;push 1 arg
	sysreq.c 	get_cvar_num	;call get_cvar_num
	stack		CELL*2			;clean-up
	xchg						;last result is now in alt
	load.s.pri	-CELL			;[pri] = maxplayers
	sub							;[pri] = maxplayers - cvar
	push.pri					;store the result - limit
	
	push.s		-CELL*3			;push limit
	push.s		-CELL			;push maxplayers
	push.s		-CELL*2			;push playersnum-1
	push.c		CELL*3			;push 3 args
	call		setVisibleSlots	;call	

	stack		3*CELL			;clean up 3 vars
	
	zero.pri					;return 0
	retn						;finalize
ENDP


.PUBLIC
	ackSignal
	plugin_init
	client_authorized
	client_disconnect