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


cinst -y tortoisesvn

# ----------------------------------------------------------------------
# --- other ---
# ----------------------------------------------------------------------

cinst -y microsoft-teams office365proplus
cinst -y openvpn