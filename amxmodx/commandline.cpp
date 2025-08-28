#include "amxmodx.h"

#define CHECK_COMMANDLINE_AVAILABILITY()                                        \
    if (!IsCommandLineAvailable())                                              \
    {                                                                           \
        LogError(amx, AMX_ERR_NATIVE, "The command line is not available.");    \
        return 0;                                                               \
    }

static bool IsCommandLineAvailable()
{
    return g_engfuncs.pfnCheckParm != nullptr;
}

// native bool:IsCommandLineAvailable();
static cell AMX_NATIVE_CALL IsCommandLineAvailable(AMX *amx, cell *params)
{
    return static_cast<cell>(IsCommandLineAvailable());
}

// native bool:FindCommandLineParam(const param[]);
static cell AMX_NATIVE_CALL FindCommandLineParam(AMX *amx, cell *params)
{
    enum {arg_count, arg_param};

    CHECK_COMMANDLINE_AVAILABILITY();

    int length;
    const char *param = get_amxstring(amx, params[arg_param], 0, length);
    if (length == 0)
    {
        return 0;
    }
    return g_engfuncs.pfnCheckParm(param, nullptr);
}

// native GetCommandLineParam(const param[], value[], maxlen, const defaultValue[]);
static cell AMX_NATIVE_CALL GetCommandLineParam(AMX *amx, cell *params)
{
    enum {arg_count, arg_param, arg_value, arg_maxlen, arg_defaultValue};

    CHECK_COMMANDLINE_AVAILABILITY();

    int length;
    char *value;
    const char *param = get_amxstring(amx, params[arg_param], 0, length);
    if (length == 0 || !g_engfuncs.pfnCheckParm(param, &value))
    {
        return set_amxstring(amx, params[arg_value], get_amxstring(amx, params[arg_defaultValue], 0, length), params[arg_maxlen]);
    }

    return set_amxstring(amx, params[arg_value], value, params[arg_maxlen]);
}

// native GetCommandLineParamInt(const param[], defaultValue);
static cell AMX_NATIVE_CALL GetCommandLineParamInt(AMX *amx, cell *params)
{
    enum {arg_count, arg_param, arg_defaultValue};
    
    CHECK_COMMANDLINE_AVAILABILITY();

    int length;
    char *value;
    const char *param = get_amxstring(amx, params[arg_param], 0, length);
    if (length == 0 || !g_engfuncs.pfnCheckParm(param, &value))
    {
        return params[arg_defaultValue];
    }

    return atoi(value);
}

// native Float:GetCommandLineParamFloat(const param[], Float:defaultValue);
static cell AMX_NATIVE_CALL GetCommandLineParamFloat(AMX *amx, cell *params)
{
    enum {arg_count, arg_param, arg_defaultValue};

    CHECK_COMMANDLINE_AVAILABILITY();

    int length;
    char *value;
    const char *param = get_amxstring(amx, params[arg_param], 0, length);
    if (length == 0 || !g_engfuncs.pfnCheckParm(param, &value))
    {
        return params[arg_defaultValue];
    }

    float fValue = atof(value);
    return amx_ftoc(fValue);
}

AMX_NATIVE_INFO g_CommandLineNatives[] =
{
    {"IsCommandLineAvailable",      IsCommandLineAvailable  },
    {"FindCommandLineParam",        FindCommandLineParam    },
    {"GetCommandLineParam",         GetCommandLineParam     },
    {"GetCommandLineParamInt",      GetCommandLineParamInt  },
    {"GetCommandLineParamFloat",    GetCommandLineParamFloat},
    {nullptr,                       nullptr}
};
