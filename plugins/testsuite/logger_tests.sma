#include <amxmodx>
#include <logger>

static const TEST[][] = {
    "FAILED",
    "PASSED"
};

static tests, passed;

public plugin_init() {
    tests = passed = 0;

    log_amx("Testing LoggerCreate");
    log_amx("LoggerCreate(...)");
    new Logger:logger = LoggerCreate();
    new bool: isEqual = logger > Invalid_Logger;
    log_amx("\t%s - LoggerCreate(...) != Invalid_Logger; actual = %d", TEST[isEqual], logger);
    tests++;
    if (isEqual) passed++;

    test_natives(logger);
    test_logger_min_verbosity_cvar();
    test_logger_tests_logger_verbosity_cvar(logger);

    log_amx("Testing LoggerDestroy");
    log_amx("LoggerDestroy(%d)", logger);
    isEqual = LoggerDestroy(logger);
    log_amx("\t%s - LoggerDestroy(%d) == true; actual = %d", TEST[isEqual], logger, isEqual);
    tests++;
    if (isEqual) passed++;
    isEqual = logger == Invalid_Logger;
    log_amx("\t%s - logger == Invalid_Logger; actual = %d", TEST[isEqual], logger);
    tests++;
    if (isEqual) passed++;

    log_amx("Finished Logger tests: %s (%d/%d)", TEST[tests == passed], passed, tests);
}

test_natives(Logger: logger) {
    log_amx("Testing logger natives");
    new temp[32];

    log_amx("LoggerSetVerbosity(%d, Severity_None)", logger);
    LoggerSetVerbosity(logger, Severity_None);
    new bool: isEqual = LoggerGetVerbosity(logger) == Severity_None;
    log_amx("\t%s - LoggerGetVerbosity(%d) == Severity_None; actual = %d", TEST[isEqual], logger, LoggerGetVerbosity(logger));
    tests++;
    if (isEqual) passed++;
    
    log_amx("LoggerSetVerbosity(%d, Severity_Error)", logger);
    LoggerSetVerbosity(logger, Severity_Error);
    isEqual = LoggerGetVerbosity(logger) == Severity_Error;
    log_amx("\t%s - LoggerGetVerbosity(%d) == Severity_Error; actual = %d", TEST[isEqual], logger, LoggerGetVerbosity(logger));
    tests++;
    if (isEqual) passed++;
    
    log_amx("LoggerSetNameFormat(%d, \"test1\")", logger);
    LoggerSetNameFormat(logger, "test1");
    LoggerGetNameFormat(logger, temp, charsmax(temp));
    isEqual = bool:equal(temp, "test1");
    log_amx("\t%s - LoggerGetNameFormat(%d) == \"test1\"; actual = \"%s\"", TEST[isEqual], logger, temp);
    tests++;
    if (isEqual) passed++;
    
    log_amx("LoggerSetNameFormat(%d, \"test2\")", logger);
    LoggerSetNameFormat(logger, "test2");
    LoggerGetNameFormat(logger, temp, charsmax(temp));
    isEqual = bool:equal(temp, "test2");
    log_amx("\t%s - LoggerGetNameFormat(%d) == \"test2\"; actual = \"%s\"", TEST[isEqual], logger, temp);
    tests++;
    if (isEqual) passed++;
    
    log_amx("LoggerSetMessageFormat(%d, \"test1\")", logger);
    LoggerSetMessageFormat(logger, "test1");
    LoggerGetMessageFormat(logger, temp, charsmax(temp));
    isEqual = bool:equal(temp, "test1");
    log_amx("\t%s - LoggerGetMessageFormat(%d) == \"test1\"; actual = \"%s\"", TEST[isEqual], logger, temp);
    tests++;
    if (isEqual) passed++;
    
    log_amx("LoggerSetMessageFormat(%d, \"test2\")", logger);
    LoggerSetMessageFormat(logger, "test2");
    LoggerGetMessageFormat(logger, temp, charsmax(temp));
    isEqual = bool:equal(temp, "test2");
    log_amx("\t%s - LoggerGetMessageFormat(%d) == \"test2\"; actual = \"%s\"", TEST[isEqual], logger, temp);
    tests++;
    if (isEqual) passed++;
    
    log_amx("LoggerSetDateFormat(%d, \"test1\")", logger);
    LoggerSetDateFormat(logger, "test1");
    LoggerGetDateFormat(logger, temp, charsmax(temp));
    isEqual = bool:equal(temp, "test1");
    log_amx("\t%s - LoggerGetDateFormat(%d) == \"test1\"; actual = \"%s\"", TEST[isEqual], logger, temp);
    tests++;
    if (isEqual) passed++;
    
    log_amx("LoggerSetDateFormat(%d, \"test2\")", logger);
    LoggerSetDateFormat(logger, "test2");
    LoggerGetDateFormat(logger, temp, charsmax(temp));
    isEqual = bool:equal(temp, "test2");
    log_amx("\t%s - LoggerGetDateFormat(%d) == \"test2\"; actual = \"%s\"", TEST[isEqual], logger, temp);
    tests++;
    if (isEqual) passed++;
    
    log_amx("LoggerSetTimeFormat(%d, \"test1\")", logger);
    LoggerSetTimeFormat(logger, "test1");
    LoggerGetTimeFormat(logger, temp, charsmax(temp));
    isEqual = bool:equal(temp, "test1");
    log_amx("\t%s - LoggerGetTimeFormat(%d) == \"test1\"; actual = \"%s\"", TEST[isEqual], logger, temp);
    tests++;
    if (isEqual) passed++;
    
    log_amx("LoggerSetTimeFormat(%d, \"test2\")", logger);
    LoggerSetTimeFormat(logger, "test2");
    LoggerGetTimeFormat(logger, temp, charsmax(temp));
    isEqual = bool:equal(temp, "test2");
    log_amx("\t%s - LoggerGetTimeFormat(%d) == \"test2\"; actual = \"%s\"", TEST[isEqual], logger, temp);
    tests++;
    if (isEqual) passed++;
    
    log_amx("LoggerSetPathFormat(%d, \"test1\")", logger);
    LoggerSetPathFormat(logger, "test1");
    LoggerGetPathFormat(logger, temp, charsmax(temp));
    isEqual = bool:equal(temp, "test1");
    log_amx("\t%s - LoggerGetPathFormat(%d) == \"test1\"; actual = \"%s\"", TEST[isEqual], logger, temp);
    tests++;
    if (isEqual) passed++;
    
    log_amx("LoggerSetPathFormat(%d, \"test2\")", logger);
    LoggerSetPathFormat(logger, "test2");
    LoggerGetPathFormat(logger, temp, charsmax(temp));
    isEqual = bool:equal(temp, "test2");
    log_amx("\t%s - LoggerGetPathFormat(%d) == \"test2\"; actual = \"%s\"", TEST[isEqual], logger, temp);
    tests++;
    if (isEqual) passed++;
    
    log_amx("LoggerSetTraceFormat(%d, \"test1\")", logger);
    LoggerSetTraceFormat(logger, "test1");
    LoggerGetTraceFormat(logger, temp, charsmax(temp));
    isEqual = bool:equal(temp, "test1");
    log_amx("\t%s - LoggerGetTraceFormat(%d) == \"test1\"; actual = \"%s\"", TEST[isEqual], logger, temp);
    tests++;
    if (isEqual) passed++;
    
    log_amx("LoggerSetTraceFormat(%d, \"test2\")", logger);
    LoggerSetTraceFormat(logger, "test2");
    LoggerGetTraceFormat(logger, temp, charsmax(temp));
    isEqual = bool:equal(temp, "test2");
    log_amx("\t%s - LoggerGetTraceFormat(%d) == \"test2\"; actual = \"%s\"", TEST[isEqual], logger, temp);
    tests++;
    if (isEqual) passed++;
}

test_logger_min_verbosity_cvar() {
    log_amx("Testing logger_min_verbosity cvar");
    new value = get_cvar_num("logger_min_verbosity");
    new bool: isEqual = value == any:LoggerGetVerbosity(All_Loggers);
    log_amx("%s - get_cvar_num(\"logger_min_verbosity\") == LoggerGetVerbosity(All_Loggers)", TEST[isEqual]);
    tests++;
    if (isEqual) passed++;

    log_amx("Setting logger_min_verbosity = 0");
    set_cvar_num("logger_min_verbosity", 0);
    value = get_cvar_num("logger_min_verbosity");
    isEqual = value == any:LoggerGetVerbosity(All_Loggers);
    log_amx("\t%s - get_cvar_num(\"logger_min_verbosity\") == LoggerGetVerbosity(All_Loggers)", TEST[isEqual]);
    tests++;
    if (isEqual) passed++;

    log_amx("Setting logger_min_verbosity = 1");
    set_cvar_num("logger_min_verbosity", 1);
    value = get_cvar_num("logger_min_verbosity");
    isEqual = value == any:LoggerGetVerbosity(All_Loggers);
    log_amx("\t%s - get_cvar_num(\"logger_min_verbosity\") == LoggerGetVerbosity(All_Loggers)", TEST[isEqual]);
    tests++;
    if (isEqual) passed++;

    log_amx("Setting logger_min_verbosity = 101");
    set_cvar_num("logger_min_verbosity", 101);
    value = get_cvar_num("logger_min_verbosity");
    isEqual = value == any:LoggerGetVerbosity(All_Loggers);
    log_amx("\t%s - get_cvar_num(\"logger_min_verbosity\") == LoggerGetVerbosity(All_Loggers)", TEST[isEqual]);
    tests++;
    if (isEqual) passed++;

    log_amx("Setting logger_min_verbosity = 201");
    set_cvar_num("logger_min_verbosity", 201);
    value = get_cvar_num("logger_min_verbosity");
    isEqual = value == any:LoggerGetVerbosity(All_Loggers);
    log_amx("\t%s - get_cvar_num(\"logger_min_verbosity\") == LoggerGetVerbosity(All_Loggers)", TEST[isEqual]);
    tests++;
    if (isEqual) passed++;

    log_amx("Setting logger_min_verbosity = 301");
    set_cvar_num("logger_min_verbosity", 301);
    value = get_cvar_num("logger_min_verbosity");
    isEqual = value == any:LoggerGetVerbosity(All_Loggers);
    log_amx("\t%s - get_cvar_num(\"logger_min_verbosity\") == LoggerGetVerbosity(All_Loggers)", TEST[isEqual]);
    tests++;
    if (isEqual) passed++;
}

test_logger_tests_logger_verbosity_cvar(Logger: logger) {
    log_amx("Testing logger_tests_logger_verbosity cvar");
    new value = get_cvar_num("logger_tests_logger_verbosity");
    new bool: isEqual = value == any:LoggerGetVerbosity(logger);
    // Expected it may fail, initial value hasn't gotten set yet
    //log_amx("%s - get_cvar_num(\"logger_tests_logger_verbosity\") == LoggerGetVerbosity(%d); %d == %d", TEST[isEqual], logger, value, LoggerGetVerbosity(logger));
    //tests++;
    //if (isEqual) passed++;

    log_amx("Setting logger_tests_logger_verbosity = 0");
    set_cvar_num("logger_tests_logger_verbosity", 0);
    value = get_cvar_num("logger_tests_logger_verbosity");
    isEqual = value == any:LoggerGetVerbosity(logger);
    log_amx("\t%s - get_cvar_num(\"logger_tests_logger_verbosity\") == LoggerGetVerbosity(%d)", TEST[isEqual], logger);
    tests++;
    if (isEqual) passed++;

    log_amx("Setting logger_tests_logger_verbosity = 1");
    set_cvar_num("logger_tests_logger_verbosity", 1);
    value = get_cvar_num("logger_tests_logger_verbosity");
    isEqual = value == any:LoggerGetVerbosity(logger);
    log_amx("\t%s - get_cvar_num(\"logger_tests_logger_verbosity\") == LoggerGetVerbosity(%d)", TEST[isEqual], logger);
    tests++;
    if (isEqual) passed++;

    log_amx("Setting logger_tests_logger_verbosity = 101");
    set_cvar_num("logger_tests_logger_verbosity", 101);
    value = get_cvar_num("logger_tests_logger_verbosity");
    isEqual = value == any:LoggerGetVerbosity(logger);
    log_amx("\t%s - get_cvar_num(\"logger_tests_logger_verbosity\") == LoggerGetVerbosity(%d)", TEST[isEqual], logger);
    tests++;
    if (isEqual) passed++;

    log_amx("Setting logger_tests_logger_verbosity = 201");
    set_cvar_num("logger_tests_logger_verbosity", 201);
    value = get_cvar_num("logger_tests_logger_verbosity");
    isEqual = value == any:LoggerGetVerbosity(logger);
    log_amx("\t%s - get_cvar_num(\"logger_tests_logger_verbosity\") == LoggerGetVerbosity(%d)", TEST[isEqual], logger);
    tests++;
    if (isEqual) passed++;

    log_amx("Setting logger_tests_logger_verbosity = 301");
    set_cvar_num("logger_tests_logger_verbosity", 301);
    value = get_cvar_num("logger_tests_logger_verbosity");
    isEqual = value == any:LoggerGetVerbosity(logger);
    log_amx("\t%s - get_cvar_num(\"logger_tests_logger_verbosity\") == LoggerGetVerbosity(%d)", TEST[isEqual], logger);
    tests++;
    if (isEqual) passed++;
}

/*public OnLoggerCreated(
        const Logger: logger,
        const Severity: verbosity,
        const name[],
        const nameFormat[],
        const msgFormat[],
        const dateFormat[],
        const timeFormat[],
        const pathFormat[],
        const traceFormat[]) {
    server_print("logger = %d", logger);
    server_print("verbosity = %d", verbosity);
    server_print("name = %s", name);
    server_print("nameFormat = %s", nameFormat);
    server_print("msgFormat = %s", msgFormat);
    server_print("dateFormat = %s", dateFormat);
    server_print("timeFormat = %s", timeFormat);
    server_print("pathFormat = %s", pathFormat);
    server_print("traceFormat = %s", traceFormat);
}*/