/* AMX Mod X Backwards Compatibility
 *
 * by the AMX Mod X Development Team
 *
 * This file is provided as is (no warranties).
 */

#if defined _amxmod_translator_included
  #endinput
#endif
#define _amxmod_translator_included

#define _translator_included

#include <amxmodx>
#include <amxmod>
#include <amxmisc>

//From AMX Mod.  This is implemented in Core due to the nature of the 
// translation engine and what AMX Mod did.
/* Translation backend, used by _T (since natives can't return arrays). */
native translate(const string[], destid=-1, forcelang=-1);

stock _T(const string[], destid=-1, forcelang=-1)
{
	new TranslationResult[2] = {0, 0};
	TranslationResult[0] = translate(string, destid, forcelang);
	return TranslationResult;
}

stock load_translations(const file[])
{
	static dir[255], path[255];
	get_datadir(dir, 254);
	
	format(path, 254, "%s/amxmod-lang/%s.txt", dir, file);
	new fp
	if (!(fp=fopen(path, "r")))
	{
		abort(AMX_ERR_NATIVE, "Could not find file: %s", path);
		return 0;
	}
	
	static buffer[1024];
	new lang[3];
	new TransKey:bad_key = TransKey:-1;
	new TransKey:cur_key = bad_key;
	new len;
	while (!feof(fp))
	{
		buffer[0] = 0;
		fgets(fp, buffer, 1023);
		len = strlen(buffer);
		if (len == 0)
		{
			continue;
		}
		if (isspace(buffer[len-1]))
		{
			buffer[--len] = 0;
		}
		if (buffer[0] == '"')
		{
			remove_quotes(buffer);
			cur_key = CreateLangKey(buffer);
			AddTranslation("en", cur_key, buffer);
			continue;
		}
		if (isspace(buffer[0]))
		{
			continue;
		}
		if ((cur_key != bad_key) && (buffer[2] == ':' && buffer[3] == '"'))
		{
			lang[0] = buffer[0];
			lang[1] = buffer[1];
			lang[2] = 0;
			remove_quotes(buffer[3]);
			AddTranslation(lang, cur_key, buffer[3]);
		}
	}
	
	fclose(fp);
	
	return 1;
}
