#ifndef FRAMEACTION_H
#define FRAMEACTION_H

#include "amxmodx.h"
#include "CQueue.h"

class CFrameActionMngr
{
public:

	class CFrameAction
	{
	public:
		CFrameAction(int callbackForward, cell callbackData) :
			m_callbackForward(callbackForward),
			m_callbackData(callbackData)
		{

		}

		~CFrameAction()
		{
			unregisterSPForward(m_callbackForward);
		}

		void Execute()
		{
			executeForwards(m_callbackForward, m_callbackData);
		}

	public:
		int m_callbackForward;
		cell m_callbackData;
	};

public:

	void AddFrameAction(int callbackForward, cell callbackData)
	{
		m_requestedFrames.push(new CFrameAction(callbackForward, callbackData));
	}

	void ExecuteFrameCallbacks()
	{
		// In case a frame callback requests another frame, newly added frames won't be executed this way
		int callbacksToRun = m_requestedFrames.size();
		while (callbacksToRun--)
		{
			CFrameAction *action = m_requestedFrames.front();
			m_requestedFrames.pop();

			action->Execute();
			delete action;
		}
	}

private:
	CQueue<CFrameAction *> m_requestedFrames;

};

#endif // FRAMEACTION_H