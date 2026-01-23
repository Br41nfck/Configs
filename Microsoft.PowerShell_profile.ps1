# C:\Users\%username%\Documents\WindowsPowerShell\Microsoft.PowerShell_profile.ps1

# CREATE PROFILE
#$profileDir = Join-Path $env:USERPROFILE "Documents\WindowsPowerShell"
#$profileFile = Join-Path $profileDir "Microsoft.PowerShell_profile.ps1"

#if (-not (Test-Path $profileDir)) {
#    New-Item -Path $profileDir -ItemType Directory -Force | Out-Null
#}

#if (-not (Test-Path $profileFile)) {
#    New-Item -Path $profileFile -ItemType File -Force | Out-Null
#}

# RELOAD PROFILE 
# . $PROFILE

# CLEAR SCREEN
Clear-Host

### PROGRAMS ###
function npp {
    & "C:\Program Files (x86)\Notepad++\notepad++.exe" @args
}

function ff {
	& "C:\Program Files\Mozilla Firefox\firefox.exe"
}

function google { 
	& "C:\Program Files\Google\Chrome\Application\chrome.exe"
}

function tc {
	& "C:\Total Commander Extended\Totalcmd64.exe"
}

### SYSTEM ###

function ev {
	& "eventvwr.exe"
}

function wdscon {
	& "WdsMgmt.msc"
}

function dhcpcon {
	& "dhcpmgmt.msc"
}

function srvcon {
	& "ServerManager.exe"
}

function top {
    Get-Process | Sort-Object CPU -Descending | Select-Object -First 15
}

function windef {
    param($Name)
    Start-Process "windowsdefender://$Name"
}

#Home					 		- start windowsdefender:
#Virus & threat protection 		- start windowsdefender://threat
#Account protection				- start windowsdefender://account
#Firewall & network protection 	- start windowsdefender://network
#App & browser control			- start windowsdefender://appbrowser
#Device security				- start windowsdefender://devicesecurity
#Device performance & health	- start windowsdefender://perfhealth
#Family options					- start windowsdefender://family
#Protection history				- start windowsdefender://history
#Security providers				- start windowsdefender://providers
#Notifications					- start windowsdefender://settings


### NET ###
function ip {
    Get-NetIPAddress | Where-Object {$_.AddressFamily -eq "IPv4"}
}

function gw {
    Get-NetRoute -DestinationPrefix "0.0.0.0/0"
}

function dns {
    Get-DnsClientServerAddress -AddressFamily IPv4
}

function flushdns {
    Clear-DnsClientCache
}

function port {
    param($Port)
    Get-NetTCPConnection -LocalPort $Port -State Listen
}

function testport {
    param($Host, $Port)
    Test-NetConnection -ComputerName $Host -Port $Port
}

function dhcp {
    Get-DhcpServerv4Scope
}

function dhl {
    param($ScopeId)
    Get-DhcpServerv4Lease -ScopeId $ScopeId
}

function dnsr {
    param($Name)
    Resolve-DnsName $Name
}

function errors {
    Get-WinEvent -LogName System -MaxEvents 20 | Where-Object LevelDisplayName -eq "Error"
}

function logon {
    Get-WinEvent -FilterHashtable @{LogName='Security'; Id=4624} -MaxEvents 10
}

function size {
    param($Path=".")
    Get-ChildItem $Path -Recurse -ErrorAction SilentlyContinue |
    Measure-Object -Property Length -Sum |
    Select-Object @{n="GB";e={[math]::Round($_.Sum/1GB,2)}}
}

function takeownx {
    param($Path)
    takeown /f "$Path" /r /d y
    icacls "$Path" /grant Administrators:F /t
}

### SERVICES ###
function getsvc {
    param($Name)
    Get-Service -Name *$Name*
}

function rssvc {
    param($Name)
    Restart-Service -Name $Name -Force
}

function rswds {
	Restart-Service WDSServer
}

function rsdhcp {
	Restart-Service DHCPServer
}

### MISC ### 
function pg {
	& ping 192.168.50.1 -n 4
}

function ll {
    Get-ChildItem -Force
}

function la {
    Get-ChildItem -Force -Recurse
}

function clsx {
    Clear-Host
}

Set-Alias np npp
Set-Alias psx Get-Process
Set-Alias ggl google