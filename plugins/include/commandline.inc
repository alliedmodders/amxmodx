#if defined _commandline_included
    #endinput
#endif
#define _commandline_included


/**
 * Determines if the command line can be queried for parameters.
 * 
 * @note If this returns false other CommandLine natives will throw an error when used.
 *
 * @return              True if the command line is available; otherwise, false.
 */
native bool:IsCommandLineAvailable();

/**
 * Determines if a specific command line parameter is present.
 *
 * @param param         The command line parameter to test.
 * @return              True if the command line parameter is specified; otherwise, false.
 * @error               If the command line is not available.
 */
native bool:FindCommandLineParam(const param[]);

/**
 * Gets the value of a command line parameter the server was launched with.
 *
 * @param param         The command line parameter to get the value of.
 * @param value         Buffer to store the parameter value in.
 * @param maxlen        Maximum length of the value buffer.
 * @param defaultValue  The default value to return if the parameter wasn't specified.
 * @return              Number of characters written to value buffer.
 * @error               If the command line is not available.
 */
native GetCommandLineParam(const param[], value[], maxlen, const defaultValue[] = "");

/**
 * Gets the integer value of a command line parameter the server was launched with.
 *
 * @param param         The command line parameter to get the value of.
 * @param defaultValue  The default value to return if the parameter wasn't specified.
 * @return              The integer value of the command line parameter value.
 * @error               If the command line is not available.
 */
native GetCommandLineParamInt(const param[], defaultValue = 0);

/**
 * Gets the floating point value of a command line parameter the server was launched with.
 *
 * @param param         The command line parameter to get the value of.
 * @param defaultValue  The default value to return if the parameter wasn't specified.
 * @return              The floating point value of the command line parameter value.
 * @error               If the command line is not available.
 */
native Float:GetCommandLineParamFloat(const param[], Float:defaultValue = 0.0);
