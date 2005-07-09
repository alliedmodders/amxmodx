#ifndef _INCLUDE_ESF_PDATA_H
#define _INCLUDE_ESF_PDATA_H

//dword aligned offsets!
#ifdef __linux__
#define EXOFF_D		5
#else
#define	EXOFF_D		0
#endif

#define ESF_POWERLEVEL1		460 + EXOFF_D
#define	ESF_POWERLEVEL2		461 + EXOFF_D

#define ESF_SPEED1			459 + EXOFF_D
#define ESF_SPEED2			462 + EXOFF_D


#endif //_INCLUDE_ESF_PDATA_H
