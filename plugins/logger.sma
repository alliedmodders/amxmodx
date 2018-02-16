#define VERSION_STRING "1.0.0"

#include <amxmodx>
#include <logger>

static g_MinVerbosityCvar;

static temp[2];
static Trie: cvarMap = Invalid_Trie;

public plugin_init() {
    register_plugin(
            .plugin_name = "Logger CVar Manager",
            .version = getBuildId(),
            .author = "Tirant");

    new defaultValue[32];
    num_to_str(any:DEFAULT_LOGGER_VERBOSITY, defaultValue, charsmax(defaultValue));

    g_MinVerbosityCvar = create_cvar(
            .name = "logger_min_verbosity",
            .string = defaultValue,
            .flags = FCVAR_NONE,
            .description = "Controls the minimum severity a message must have \
                    in order to be logged across all loggers",
            .has_min = true,
            .min_val = float(any:Severity_None),
            .has_max = false);
    
    new curValue[32];
    get_pcvar_string(g_MinVerbosityCvar, curValue, charsmax(curValue));
    LoggerSetVerbosity(All_Loggers, toSeverity(str_to_num(curValue)));
    hook_cvar_change(g_MinVerbosityCvar, "onMinVerbosityCvarChanged");
}

getBuildId() {
    new buildId[32];
    formatex(buildId, charsmax(buildId), "%s [%s]", VERSION_STRING, __DATE__);
    return buildId;
}

public onMinVerbosityCvarChanged(pcvar, const old_value[], const new_value[]) {
    assert pcvar == g_MinVerbosityCvar;
    LoggerSetVerbosity(All_Loggers, toSeverity(str_to_num(new_value)));
}

public OnLoggerCreated(
        const Logger: logger,
        const Severity: verbosity,
        const name[],
        const nameFormat[],
        const msgFormat[],
        const dateFormat[],
        const timeFormat[],
        const pathFormat[],
        const traceFormat[]) {
    new cvarName[32];
    formatex(cvarName, charsmax(cvarName), "%s_logger_verbosity", name);

    new defaultValue[32];
    num_to_str(any:verbosity, defaultValue, charsmax(defaultValue));

    new cvarDescription[256];
    formatex(cvarDescription, charsmax(cvarDescription), "Controls the minimum \
            severity that the %s logger will log",
            name);

    new cvarVerbosity = create_cvar(
            .name = cvarName,
            .string = defaultValue,
            .flags = FCVAR_NONE,
            .description = cvarDescription,
            .has_min = true,
            .min_val = float(any:Severity_None),
            .has_max = false);

    temp[0] = cvarVerbosity;
    if (cvarMap == Invalid_Trie) {
        cvarMap = TrieCreate();
    }

    TrieSetCell(cvarMap, temp, logger);

    new curValue[32];
    get_pcvar_string(cvarVerbosity, curValue, charsmax(curValue));
    onVerbosityCvarChanged(cvarVerbosity, NULL_STRING, curValue);
    hook_cvar_change(cvarVerbosity, "onVerbosityCvarChanged");
}

public onVerbosityCvarChanged(pcvar, const old_value[], const new_value[]) {
    assert cvarMap != Invalid_Trie;

    temp[0] = pcvar;
    new Logger: logger;
    assert TrieGetCell(cvarMap, temp, logger);
    assert logger > Invalid_Logger;
    LoggerSetVerbosity(logger, toSeverity(str_to_num(new_value)));
}