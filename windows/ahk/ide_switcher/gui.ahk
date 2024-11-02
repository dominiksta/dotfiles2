#Include %A_ScriptDir%\util.ahk
#Include %A_ScriptDir%\config.ahk

ShowGui() {
    Gui ideswtchgui:Font, s12, Consolas
    Gui ideswtchgui:Color, 181818

    Gui ideswtchgui:Add, Text, cWhite x32 y16 w80 h23, &A
    Gui ideswtchgui:Add, Text, cWhite x32 y48 w80 h23, &S
    Gui ideswtchgui:Add, Text, cWhite x32 y80 w80 h23, &D
    Gui ideswtchgui:Add, Text, cWhite x32 y112 w80 h23, &F

    Gui ideswtchgui:Add, Text, cWhite x32 y160 w80 h23, &H
    Gui ideswtchgui:Add, Text, cWhite x32 y192 w80 h23, &J
    Gui ideswtchgui:Add, Text, cWhite x32 y224 w80 h23, &K
    Gui ideswtchgui:Add, Text, cWhite x32 y256 w80 h23, &L

    Windows := GetAllTitles()
    LogFile("ShowGui")

    for index in Windows {
        prefix := Windows[a_index][1]
        title := Windows[a_index][2]

        offset := 16 + (index - 1) * 32

        Gui ideswtchgui:Add, Text, x112 y%offset% w422 h23 cWhite +0x200, %prefix%: %title%
    }

    Gui ideswtchgui:Show, w620 h300, IDE Switcher
    
    Return
}

ActivateAndExit(hWnd) {
    LogFile("Activating " hWnd)
	WinGetTitle, Title, ahk_id %hWnd%
    LogFile("Activating " Title " (" hWnd ")")
    WinActivate, ahk_id %hWnd%
    Gui ideswtchgui:Destroy
    return
}

IdeswtchguiGuiEscape:
IdeswtchguiGuiClose:
    Gui, ideswtchgui:Hide
