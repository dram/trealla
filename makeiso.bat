set USE_SSL64=0
if "%Platform%" == "x64" set USE_SSL64=1
nmake ISO_ONLY=1 USE_SSL=0 -f WINmakefile
