set USE_SSL64=0
if "%Platform%" == "x64" set USE_SSL64=1
nmake USE_SSL=1 USE_SSL64=%USE_SSL64% -f WINmakefile
