REM Install packages, this takes a while
winget import %APPDATA%\.dotfiles\winget\packages.json

REM Should be in PATH
mkdir C:\tools

REM Create links
@FOR /F "tokens=* USEBACKQ" %%F IN (`dir /S /B C:\progra~1\Emacs\*runemacs.exe`) DO @(SET EMACS_PATH=%%F)
mklink C:\tools\runemacs.exe %EMACS_PATH%
mklink /J %APPDATA%\.emacs.d %APPDATA%\.dotfiles\.emacs.d

pause
