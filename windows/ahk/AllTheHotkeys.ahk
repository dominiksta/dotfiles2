#NoEnv                       ; Recommended for performance and compatibility with future AutoHotkey releases.
; #Warn                      ; Enable warnings to assist with detecting common errors.
SendMode Input               ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.

; dependencies:
; ProcessInfo.ahk  -- identify window by process name (exe file)
; nircmd.exe       -- audio control
; emacs

#Include %A_ScriptDir%\ProcessInfo.ahk

; Control       = ^
; Meta          = !
; Shift         = +
; Super (Win)   = #
; ContextMenu   = AppsKey
; I bind the left Windows-Key to the "AppsKey" in the registry, but remap it again in any
; other application than emacs:
#IfWinActive fp@emacs
    AppsKey::AppsKey
return
#IfWinActive
AppsKey::Rwin

;------------------------------------------------------------
; Audio Control
;------------------------------------------------------------

^!PgDn::
	Run, nircmd.exe setdefaultsounddevice Saffire
	Run, nircmd.exe setdefaultsounddevice Saffire 2
return

^!PgUp::
	Run, nircmd.exe setdefaultsounddevice Realtek
	Run, nircmd.exe setdefaultsounddevice Realtek 2
return


;------------------------------------------------------------
; Words
;------------------------------------------------------------
+!^F9::Send https://akk.li/pics/anne/jpg

;------------------------------------------------------------
; Media Keys
;------------------------------------------------------------

; ^!Left::Send   {Media_Prev}
; ^!Down::Send   {Media_Play_Pause}
; ^!Right::Send  {Media_Next}
; +^!Down::Send  {Volume_Mute}

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

; Screenshots
+^!PrintScreen::run SnippingTool.exe


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

!+w::bringtoforegroundbyexe("waterfox.exe", "waterfox")
!Space::bringtoforegroundbytitle("fp@emacs", "C:\Users\fp\Desktop\emacs\emacs.lnk")
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
!+m::
{
        WinMinimize,A
	return
}

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
; Hide taskbar on <C-F12>
;------------------------------------------------------------
!^F9::
 WinShow,ahk_class Shell_TrayWnd
 WinShow,Start ahk_class Button
Return
!^F10::
 WinHide,ahk_class Shell_TrayWnd
 WinHide,Start ahk_class Button
Return

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