# ----------------------------------------------------------------------
# --- development ---
# ----------------------------------------------------------------------

<#
visualstudio2019community                       : Base Visual Studio
visualstudio2019buildtools                      : CLI Build Tools without IDE
visualstudio2019-workload-managedgame           : Unity Gamedev 
                                                  - also installs Unity hub
visualstudio2019-workload-manageddesktop        : .NET Desktop
visualstudio2019-workload-nativedesktop         : C++ Desktop
choco install visualstudio2019-workload-vctools : Also C++ Desktop ?
#>

cinst -y `
  visualstudio2019community visualstudio2019buildtools `
  visualstudio2019-workload-managedgame visualstudio2019-workload-manageddesktop `
  visualstudio2019-workload-nativedesktop

choco install -y dependencywalker # CLI: depends.exe, GUI: Start-Menü
choco install -y dependencies # CLI: Dependencies.exe, GUI: DependenciesGui.exe

cinst -y llvm # Completion in Emacs through clangd

cinst -y tortoisesvn sqlitebrowser sqlite.shell

# ----------------------------------------------------------------------
# --- other ---
# ----------------------------------------------------------------------

cinst -y microsoft-teams office365proplus
cinst -y openvpn