PgSQL Module for AMX Mod X
by David "BAILOPAN" Anderson
----------------------------

Installation
------------

1] Place the pgsql.cfg file in your addons/amxx/configs dir.


2] Place the module in the right place:
Linux:

Place geoip_amx_i386.so into addons/amxx/modules/
(If you want a glibc2.3 optimized binary, use geoip_amx_i686.so)
Add this line to addons/amxx/modules.ini :
geoip_amx_i386.so


Windows:

Place geoip_amx.dll into addons/ammx/modules/
Add this line to addons/amxx/modules.ini :
geoip_amx.dll


3] Scripters: Place "pgsql.inc" in addons/amxx/scripting/include

4] If you are using pgsql to store admins:
Place admin_pgsql.amx in addons/amxx/plugins/
And add this line to addons/amxx/plugins.ini:
admin_pgsql.amx