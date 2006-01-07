INCLUDES =  -I. -I.. -I../JudyCommon/ 
AM_CFLAGS = -DJUDYL @WARN_CFLAGS@ 

noinst_LTLIBRARIES = libJudyL.la libnext.la libprev.la libcount.la libinline.la

libJudyL_la_SOURCES = JudyLCascade.c JudyLTables.c JudyLCount.c JudyLCreateBranch.c JudyLDecascade.c JudyLDel.c JudyLFirst.c JudyLFreeArray.c JudyLGet.c JudyLInsArray.c JudyLIns.c JudyLInsertBranch.c JudyLMallocIF.c JudyLMemActive.c JudyLMemUsed.c 

libnext_la_SOURCES = JudyLNext.c JudyLNextEmpty.c
libnext_la_CFLAGS = $(AM_CFLAGS) -DJUDYNEXT

libprev_la_SOURCES = JudyLPrev.c JudyLPrevEmpty.c
libprev_la_CFLAGS = $(AM_CFLAGS) -DJUDYPREV

libcount_la_SOURCES = JudyLByCount.c
libcount_la_CFLAGS = $(AM_CFLAGS) -DNOSMARTJBB -DNOSMARTJBU -DNOSMARTJLB

libinline_la_SOURCES = j__udyLGet.c
libinline_la_CFLAGS = $(AM_CFLAGS) -DJUDYGETINLINE

JudyLTables.c: JudyLTablesGen.c
	$(CC) $(INCLUDES) $(AM_CFLAGS) @CFLAGS@ -o JudyLTablesGen JudyLTablesGen.c; ./JudyLTablesGen 


JudyLCascade.c: copies

copies:
	cp -f ../JudyCommon/JudyByCount.c      		JudyLByCount.c   
	cp -f ../JudyCommon/JudyCascade.c       	JudyLCascade.c
	cp -f ../JudyCommon/JudyCount.c        		JudyLCount.c
	cp -f ../JudyCommon/JudyCreateBranch.c 		JudyLCreateBranch.c
	cp -f ../JudyCommon/JudyDecascade.c    		JudyLDecascade.c
	cp -f ../JudyCommon/JudyDel.c          		JudyLDel.c
	cp -f ../JudyCommon/JudyFirst.c        		JudyLFirst.c
	cp -f ../JudyCommon/JudyFreeArray.c    		JudyLFreeArray.c
	cp -f ../JudyCommon/JudyGet.c          		JudyLGet.c
	cp -f ../JudyCommon/JudyGet.c          		j__udyLGet.c
	cp -f ../JudyCommon/JudyInsArray.c     		JudyLInsArray.c
	cp -f ../JudyCommon/JudyIns.c          		JudyLIns.c
	cp -f ../JudyCommon/JudyInsertBranch.c 		JudyLInsertBranch.c
	cp -f ../JudyCommon/JudyMallocIF.c     		JudyLMallocIF.c
	cp -f ../JudyCommon/JudyMemActive.c    		JudyLMemActive.c
	cp -f ../JudyCommon/JudyMemUsed.c      		JudyLMemUsed.c
	cp -f ../JudyCommon/JudyPrevNext.c     		JudyLNext.c
	cp -f ../JudyCommon/JudyPrevNext.c     		JudyLPrev.c
	cp -f ../JudyCommon/JudyPrevNextEmpty.c		JudyLNextEmpty.c
	cp -f ../JudyCommon/JudyPrevNextEmpty.c		JudyLPrevEmpty.c
	cp -f ../JudyCommon/JudyTables.c       	 	JudyLTablesGen.c

