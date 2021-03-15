<#

################################################################################
RUN MANUALLY AFTER SCRIPT:
################################################################################
1. Run in WSL: echo export SSH_AUTH_SOCK=/mnt/c/wsl-ssh-agent/ssh-agent.sock >> ~/.bashrc
2. Enable SSH Agent in KeepassXC:
   Settings -> SSH Agent -> Enable SSH Agent Authentication + Use OpenSSH for
   Windows instead of Paageant
3. Restart KeepassXC

Also see
- ^^20210204-215644 OpenSSH Windows Update^^
- ^^20210204-163754 OpenSSH Agent Windows zu WSL^^

#>

# Remove the built-in OpenSSH Client if it is installed
if ((Get-WindowsCapability -Online -Name OpenSSH.Client~~~~0.0.1.0).State -eq 'Installed') {
    Remove-WindowsCapability -Online -Name OpenSSH.Client~~~~0.0.1.0
}

# Install the chocolatey/git version of OpenSSH for windows
# https://chocolatey.org/packages/openssh
choco upgrade -y openssh --params "/SSHAgentFeature"

# Enable SSH Agent to automatically start
Get-Service -Name ssh-agent | Set-Service -StartupType Automatic

# --- Install wsl-ssh-agent ---

Invoke-WebRequest -Uri "https://github.com/rupor-github/wsl-ssh-agent/releases/latest/download/wsl-ssh-agent.7z" -OutFile $env:TEMP\wsl-ssh-agent.7z

mkdir -Force C:\wsl-ssh-agent
7z e $env:TEMP\wsl-ssh-agent.7z -o"C:\wsl-ssh-agent\"

schtasks /create /tn wsl-ssh-agent `
  /tr "C:\wsl-ssh-agent\wsl-ssh-agent-gui.exe -socket c:\wsl-ssh-agent\ssh-agent.sock" `
  /sc ONLOGON
