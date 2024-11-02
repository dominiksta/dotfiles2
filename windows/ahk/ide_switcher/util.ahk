
LogFile(msg) {
    FileAppend, %msg% `n, %A_ScriptDir%\log.txt
}

GetTitles(prefix, executable)
{
    out := []
    Winget, hWnds, List, ahk_exe %executable%
    Loop, % hWnds {
        hWnd := hWnds%A_Index%
        WinGetTitle, wTitle, ahk_id %hWnd%
        out.Push([prefix, wTitle, hWnd])
    }
    
    return out
}

ArrConcat(a1, a2) {
    out := []
    for index in a1 {
        out.Push(a1[a_index])
    }
    for index in a2 {
        out.Push(a2[a_index])
    }
    return out
}

GetAllTitles() {
    LogFile("GetAllTitles")
    Windows := []
    InConfig := PrefixesAndTitles()
    for index in InConfig {
        Windows := ArrConcat(Windows, GetTitles(InConfig[a_index][1], InConfig[a_index][2]))
    }
    LogFile("GetAllTitles: " InConfig[1][1])
    LogFile("GetAllTitles: " Windows[1][1])
    return Windows
}

GetProcessFullExePath(PID) {
    ; no idea who i stole this from anymore. sry :/

    hProcess := DllCall("OpenProcess",  "UInt", 0x400 | 0x10, "Int", False
                                     ,  "UInt", PID)
    PathLength = 260*2
    VarSetCapacity(FilePath, PathLength, 0)
    DllCall("Psapi.dll\GetModuleFileNameExW", "UInt", hProcess, "Int", 0
                                 , "Str", FilePath, "UInt", PathLength)
    DllCall("CloseHandle", "UInt", hProcess)

    return FilePath
}