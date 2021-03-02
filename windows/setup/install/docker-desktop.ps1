<#
Enables Hyper-V and install docker-desktop. If WSL2 was already installed
before, docker-desktop might have to be explicitly set to use the Hyper-V
backend.
#>

Enable-WindowsOptionalFeature -Online -FeatureName Microsoft-Hyper-V -All
cinst -y docker-desktop docker-compose
# Installing docker-compose seperately will install a newer version