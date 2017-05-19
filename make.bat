SET mypath=%~dp0
echo %mypath:~0,-1%
java -jar %mypath:~0,-1%\KickAssembler\KickAss.jar %mypath:~0,-1%\cynth-dsd.asm
x64 %mypath:~0,-1%\cynth-dsd.prg