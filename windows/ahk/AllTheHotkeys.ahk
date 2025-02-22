#NoEnv                       ; Recommended for performance and compatibility with future AutoHotkey releases.
; #Warn                      ; Enable warnings to assist with detecting common errors.
SendMode Input               ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.

; dependencies:
; ProcessInfo.ahk  -- identify window by process name (exe file)
; nircmd.exe       -- audio control
; emacs

#Include %A_ScriptDir%\ProcessInfo.ahk

Run, %A_ScriptDir%\ide_switcher\main.ahk

; Control       = ^
; Meta          = !
; Shift         = +
; Super (Win)   = #
; ContextMenu   = AppsKey
; I bind the left Windows-Key to the "AppsKey" in the registry, but remap it again in any
; other application than emacs:
; #IfWinActive fp@emacs
;     AppsKey::AppsKey
; return
; #IfWinActive
; AppsKey::Rwin

;------------------------------------------------------------
; Suspend All Hotkeys
;------------------------------------------------------------

!Esc::
   Suspend, Permit
   SusToggle := !SusToggle
   If (SusToggle)
   {   Suspend, On
   }
   Else
   {   Suspend Off
   }
return

;------------------------------------------------------------
; Audio Control
;------------------------------------------------------------

!+Down::Send  {Volume_Down}
!+Up::Send    {Volume_Up}
!+Right::Send {Media_Next}
!+Left::Send  {Media_Prev}
!+Space::Send {Media_Play_Pause}
!+Enter::Send {Volume_Mute}

;------------------------------------------------------------
; screenshots
;------------------------------------------------------------
; PrintScreen::
;     EnvGet temp, temp
;     runwait i_view64.exe /capture=4 /convert=%temp%\iviewscrot.png
;     run %temp%\iviewscrot.png
;     return

; !PrintScreen::
;     EnvGet temp, temp
;     runwait i_view64.exe /capture=2 /convert=%temp%\iviewscrot.png
;     run %temp%\iviewscrot.png
;     return

; +PrintScreen::
;     EnvGet temp, temp
;     runwait i_view64.exe /capture=1 /convert=%temp%\iviewscrot.png
;     run %temp%\iviewscrot.png
;     return

;------------------------------------------------------------
; Other Shortcuts
;------------------------------------------------------------

; Get Classname of active Window
+^!c::
    PID = 0
    WinGet, hWnd,, A
    DllCall("GetWindowThreadProcessId", "UInt", hWnd, "UInt *", PID)
    hProcess := DllCall("OpenProcess",  "UInt", 0x400 | 0x10, "Int", False
                                     ,  "UInt", PID)
    PathLength = 260*2
    VarSetCapacity(FilePath, PathLength, 0)
    DllCall("Psapi.dll\GetModuleFileNameExW", "UInt", hProcess, "Int", 0
                                 , "Str", FilePath, "UInt", PathLength)
    DllCall("CloseHandle", "UInt", hProcess)

    WinGetClass, class, A
    MsgBox, Class: %class% `nPath:  %FilePath%
return

;------------------------------------------------------------
; RAlt -> Umlaut (For QWERTY)
;------------------------------------------------------------
; Made by Jakub Mańczak in 2020
; manczak.net

>!e::send, {U+20AC} ; €

>!a::
if GetKeyState("CapsLock", "T") = 0
	send, {U+00E4}
else if GetKeyState("CapsLock", "T") = 1
	send, {U+00C4}
return

>!+a::
if GetKeyState("CapsLock", "T") = 0
	send, {U+00C4}
else if GetKeyState("CapsLock", "T") = 1
	send, {U+00E4}
return

>!o::
if GetKeyState("CapsLock", "T") = 0
	send, {U+00F6}
else if GetKeyState("CapsLock", "T") = 1
	send, {U+00D6}
return

>!+o::
if GetKeyState("CapsLock", "T") = 0
	send, {U+00D6}
else if GetKeyState("CapsLock", "T") = 1
	send, {U+00F6}
return

>!u::
if GetKeyState("CapsLock", "T") = 0
	send, {U+00FC}
else if GetKeyState("CapsLock", "T") = 1
	send, {U+00DC}
return

>!+u::
if GetKeyState("CapsLock", "T") = 0
	send, {U+00DC}
else if GetKeyState("CapsLock", "T") = 1
	send, {U+00FC}
return

>!s::
if GetKeyState("CapsLock", "T") = 0
	send, {U+00DF}
else if GetKeyState("CapsLock", "T") = 1
	send, {U+1E9E}
return

;------------------------------------------------------------
; Program Bindings
;------------------------------------------------------------
foregroundelisp(expr){
    SetTitleMatchMode, 2 ; exact match
    WinActivate, fp@emacs
    Run, emacsclient.exe -e "%expr%",,Hide
}

backgroundelisp(expr){
    Run, emacsclient.exe -e "%expr%",,Hide
}

bringtoforegroundbyclass(class, except){
        query = ahk_class %class%
        if WinExist(query)
                WinActivate  ; Uses the last found window.
        else
                Run %except%
        return
}

bringtoforegroundbyexe(exe, except){
        SetTitleMatchMode, 2 ; approximate match
        IfWinExist, - ahk_exe %exe%
        {
        	WinActivate, - ahk_exe %exe%
        }
        IfWinNotExist, - ahk_exe %exe%
        {
        	Run %except%
        }
        return
}

bringtoforegroundbytitle(title, except){
        SetTitleMatchMode, 2 ; exact match
        IfWinExist, %title%
        {
        	WinActivate, %title%
        }
        IfWinNotExist, %title%
        {
        	Run %except%
        }
        return
}

!+w::bringtoforegroundbyclass("MozillaWindowClass", "firefox")
; !+w::Send, {LAlt Up}{LShift Up}{LWin Down}1{LWin Up}
!+e::bringtoforegroundbyclass("TTOTAL_CMD", "C:\Program Files\totalcmd\totalcmd64.exe")
; !+p::bringtoforegroundbytitle("Plex", "C:\Program Files\Plex\Plex\Plex.exe")
!+p::bringtoforegroundbyexe("Spotify.exe", "C:\Users\dominik\AppData\Roaming\Spotify\Spotify.exe")
!+k::bringtoforegroundbyexe("KeePassXC.exe", "C:\Program Files\KeePassXC\KeePassXC.exe")
; ^+1::bringtoforegroundbyclass("PROCEXPL", "procexp")
!+g::bringtoforegroundbytitle("Fork", "")
; !Space::Run %A_ScriptDir%\ide_switcher\main.ahk
; !Space::bringtoforegroundbyclass("Emacs", "runemacs.exe")
; !Space::bringtoforegroundbytitle("emacs@wsl", "C:\Users\dominik\Source\git\dotfiles\windows\wsl\start-emacs.bat")
!,::bringtoforegroundbyclass("CASCADIA_HOSTING_WINDOW_CLASS", "wt")
; !,::bringtoforegroundbytitle("Cmder", "cmder")
; !^t::backgroundelisp("(fp/terminal-here nil)")
; !F1::foregroundelisp("(fp/external/start-program)")
!F9::foregroundelisp("(org-capture)")

;------------------------------------------------------------
; Rebinds
;------------------------------------------------------------

; Windows Workspaces
; !1::Send {LWin down}{Control down}{Left}{LWin up}{Control up}
; !2::Send {LWin down}{Control down}{Right}{LWin up}{Control up}

;------------------------------------------------------------
; Window Management
;------------------------------------------------------------

; Minimize
; !+m::
; {
;   WinMinimize,A
;   return
; }

; Maximize/Restore
!+f::
{
	WinGet,max,MinMax,A
	if max
		WinRestore,A
	else
		WinMaximize,A
	return
}

; Close
!+q::
{
	WinGetTitle, Title, A
	PostMessage, 0x112, 0xF060,,, %Title%
	return
}

;------------------------------------------------------------
; Switch primary monitor
;------------------------------------------------------------

!+^F12::Run nircmd.exe setprimarydisplay 2
!+^F11::Run nircmd.exe setprimarydisplay 1

;------------------------------------------------------------
; Remap Capslock to both Control and Escape
; from https://gist.github.com/sedm0784/4443120
;------------------------------------------------------------

g_LastCtrlKeyDownTime := 0
g_AbortSendEsc := false
g_ControlRepeatDetected := false

*CapsLock::
    if (g_ControlRepeatDetected)
    {
        return
    }

    send,{Ctrl down}
    g_LastCtrlKeyDownTime := A_TickCount
    g_AbortSendEsc := false
    g_ControlRepeatDetected := true

    return

*CapsLock Up::
    send,{Ctrl up}
    g_ControlRepeatDetected := false
    if (g_AbortSendEsc)
    {
        return
    }
    current_time := A_TickCount
    time_elapsed := current_time - g_LastCtrlKeyDownTime
    if (time_elapsed <= 170)
    {
        SendInput {Esc}
    }
    return

~*^a::
~*^b::
~*^c::
~*^d::
~*^e::
~*^f::
~*^g::
~*^h::
~*^i::
~*^j::
~*^k::
~*^l::
~*^m::
~*^n::
~*^o::
~*^p::
~*^q::
~*^r::
~*^s::
~*^t::
~*^u::
~*^v::
~*^w::
~*^x::
~*^y::
~*^z::
~*^1::
~*^2::
~*^3::
~*^4::
~*^5::
~*^6::
~*^7::
~*^8::
~*^9::
~*^0::
~*^Space::
~*^Backspace::
~*^Delete::
~*^Insert::
~*^Home::
~*^End::
~*^PgUp::
~*^PgDn::
~*^Tab::
~*^Return::
~*^,::
~*^.::
~*^/::
~*^;::
~*^'::
~*^[::
~*^]::
~*^\::
~*^-::
~*^=::
~*^`::
~*^F1::
~*^F2::
~*^F3::
~*^F4::
~*^F5::
~*^F6::
~*^F7::
~*^F8::
~*^F9::
~*^F10::
~*^F11::
~*^F12::
    g_AbortSendEsc := true
return