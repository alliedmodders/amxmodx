#ifndef FRAMEACTION_H
#define FRAMEACTION_H

#include "amxmodx.h"
#include <amtl/am-deque.h>
#include <amtl/am-autoptr.h>

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
		m_requestedFrames.append(new CFrameAction(callbackForward, callbackData));
	}

	void ExecuteFrameCallbacks()
	{
		// In case a frame callback requests another frame, newly added frames won't be executed this way
		int callbacksToRun = m_requestedFrames.length();
		while (callbacksToRun--)
		{
			ke::AutoPtr<CFrameAction> action = ke::Move(m_requestedFrames.front());
			m_requestedFrames.popFront();
			action->Execute();
		}
	}

private:
	ke::Deque<ke::AutoPtr<CFrameAction>> m_requestedFrames;

};

#endif // FRAMEACTION_H
