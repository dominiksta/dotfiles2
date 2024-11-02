#Include %A_ScriptDir%\util.ahk
#Include %A_ScriptDir%\config.ahk
#Include %A_ScriptDir%\gui.ahk

#SingleInstance Force
#Persistent ; Prevent the script from exiting automatically.

LogFile("Init")

; ----------------------------------------------------------------------
; Hook Internals
; ----------------------------------------------------------------------

Gui ideswtchbg:+AlwaysOnTop +HwndhWnd
Gui ideswtchbg:Hide

SHELL_MSG := DllCall("RegisterWindowMessage", "Str", "SHELLHOOK", "UInt")
OnMessage(SHELL_MSG, Func("ShellCallback"))

; Sets whether the shell hook is registered
SetHook(state) {
    global hWnd
    static shellHookInstalled := false
    if (!shellHookInstalled and state) {
        if (!DllCall("RegisterShellHookWindow", "Ptr", hWnd)) {
            LogFile("Failed to register shell hook")
            return false
        }
        LogFile("Registered shell hook")
        shellHookInstalled := true
    }
    else if (shellHookInstalled and !state) {
        if (!DllCall("DeregisterShellHookWindow", "Ptr", hWnd)) {
            LogFile("Failed to deregister shell hook")
            return false
        }
        LogFile("Deregistered shell hook")
        shellHookInstalled := false
    }

    return true
}

; Shell messages callback
ShellCallback(wParam, lParam) {
    ; HSHELL_WINDOWACTIVATED = 4, HSHELL_RUDEAPPACTIVATED = 0x8004
    if (wParam & 4) {
        ; lParam = hWnd of activated window

        WinGetTitle t, ahk_id %lParam%
        LogFile("active window: " t)

        OnWindowChanged(lParam)
    }
}

; ----------------------------------------------------------------------
; Main
; ----------------------------------------------------------------------

OnExit("OnExitCallback")
OnExitCallback() {
    SetHook(false)
}
SetHook(true)

LAST_IDE_WINDOW := 0

OnWindowChanged(hWnd) {
    global LAST_IDE_WINDOW
    LogFile("active hWnd: " hWnd)

    WinGet, ProcessID, PID, ahk_id %hWnd%
    LogFile("active PID: " ProcessID)

    ProcName := GetProcessFullExePath(ProcessID)
    LogFile("active exe: " ProcName)

    pt := PrefixesAndTitles()

    for index in pt {
        exe := pt[a_index][2]
        match := RegExMatch(ProcName, exe) 
        LogFile("exe: " exe ", match: " match)
        if (match != 0) {
            LAST_IDE_WINDOW := hWnd
            LogFile("Setting LAST_IDE_WINDOW = " hWnd)
        }
    }
}

IDESwitcherMain() {
    global LAST_IDE_WINDOW
    WinGet, hWnd, ID, A
    if (hWnd = LAST_IDE_WINDOW or LAST_IDE_WINDOW = 0) {
        ShowGui()
    } else {
        LogFile("Showing LAST_IDE_WINDOW " hWnd)
        WinActivate, ahk_id %LAST_IDE_WINDOW%
    }
}

; ----------------------------------------------------------------------
; Final Section: Hotkeys
; ----------------------------------------------------------------------

LogFile("Listening For Keypresses")

!Space:: IDESwitcherMain()

#IfWinActive IDE Switcher
a::ActivateAndExit(GetAllTitles()[1][3])
s::ActivateAndExit(GetAllTitles()[2][3])
d::ActivateAndExit(GetAllTitles()[3][3])
f::ActivateAndExit(GetAllTitles()[4][3])

h::ActivateAndExit(GetAllTitles()[5][3])
j::ActivateAndExit(GetAllTitles()[6][3])
k::ActivateAndExit(GetAllTitles()[7][3])
l::ActivateAndExit(GetAllTitles()[8][3])
#IfWinActive