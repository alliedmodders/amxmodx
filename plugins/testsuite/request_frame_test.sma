#include <amxmodx>
#include <fakemeta>

/*
Expected console output:

Command_Test - Frame: X
FrameCallback1 - Frame: X + 1 Data: 100
FrameCallback2 - Frame: X + 2 Data: 200
FrameCallback3 - Frame: X + 2 Data: 300
FrameCallback4 - Frame: X + 3 Data: 400
FrameCallback4 - Frame: X + 3 Data: 500
*/


new g_frameNumber = 0;

public plugin_precache()
{
	register_forward(FM_StartFrame, "OnStartFrame", false);
}

public plugin_init()
{
	register_plugin("RequestFrame() Test", "1.0.0", "KliPPy");

	register_concmd("request_frame_test", "Command_Test");
}

public OnStartFrame()
{
	g_frameNumber++;
}

public Command_Test()
{
	console_print(0, "Command_Test - Frame: %d", g_frameNumber);

	RequestFrame("FrameCallback1", 100);
}

public FrameCallback1(data)
{
	console_print(0, "FrameCallback1 - Frame: %d Data: %d", g_frameNumber, data);

	RequestFrame("FrameCallback2", 200);
	RequestFrame("FrameCallback3", 300);
}

public FrameCallback2(data)
{
	console_print(0, "FrameCallback2 - Frame: %d Data: %d", g_frameNumber, data);
	RequestFrame("FrameCallback4", 400);
}

public FrameCallback3(data)
{
	console_print(0, "FrameCallback3 - Frame: %d Data: %d", g_frameNumber, data);
	RequestFrame("FrameCallback4", 500);
}

public FrameCallback4(data)
{
	console_print(0, "FrameCallback4 - Frame: %d Data: %d", g_frameNumber, data);
}

