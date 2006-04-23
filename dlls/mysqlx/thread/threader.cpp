// threader.cpp : Defines the entry point for the console application.
//
#include <stdio.h>
#include "WinThreads.h"
#include "ThreadWorker.h"

using namespace SourceMod;

unsigned int g_count = 0;

class Sai : public IThread
{
public:
	virtual void RunThread(IThreadHandle *pHandle)
	{
		printf("[%02d] Ran at: %d\n", ++g_count, GetTickCount());
	}
	virtual void OnTerminate(IThreadHandle *pHandle, bool cancel)
	{
	}
};

int main(int argc, char* argv[])
{
	WinThreader kWt;
	ThreadWorker kWorker(&kWt, 0);
	Sai sai;

	printf("Queueing three threads:\n");
	kWorker.Start();
	kWorker.MakeThread(&sai);
	kWorker.MakeThread(&sai);
	kWorker.MakeThread(&sai);
	printf("Waiting 10 seconds...\n");
	Sleep(10000);
	printf("Done waiting, adding 5 threads...\n");
	kWorker.MakeThread(&sai);
	kWorker.MakeThread(&sai);
	kWorker.MakeThread(&sai);
	kWorker.MakeThread(&sai);
	kWorker.MakeThread(&sai);
	printf("Pausing...\n");
	kWorker.Pause();
	printf("Sleeping for 10 seconds... \n");
	Sleep(10000);
	printf("Unpausing... \n");
	kWorker.Unpause();
	printf("Sleeping for 10 seconds... \n");
	Sleep(10000);
}

