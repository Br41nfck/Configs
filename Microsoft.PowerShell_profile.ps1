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
    & "C:\Program Files\Notepad++\notepad++.exe" @args
}

function vs {
	& "C:\Program Files\Microsoft Visual Studio\18\Insiders\Common7\IDE\devenv.exe" @args
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

function syspath {
	$env:Path -split ";"
}

function windef {
    param($Name)
    Start-Process "windowsdefender://$Name"
}

function Get-Sysinternals {

    # Путь для установки Sysinternals
    $installPath = "C:\Sysinternals"

    # URL последней версии Sysinternals Suite
    $url = "https://download.sysinternals.com/files/SysinternalsSuite.zip"

    # Создаём папку, если не существует
    if (-not (Test-Path -Path $installPath)) {
        New-Item -Path $installPath -ItemType Directory | Out-Null
    }

    # Путь для временного скачанного архива
    $tempZip = "$env:TEMP\SysinternalsSuite.zip"

    # Скачиваем архив
    Write-Host "Downloading Sysinternals Suite..."
    Invoke-WebRequest -Uri $url -OutFile $tempZip

    # Распаковываем архив
    Write-Host "Extracting archive..."
    Expand-Archive -Path $tempZip -DestinationPath $installPath -Force
    Remove-Item $tempZip

    # Проверка прав администратора
    $isAdmin = ([Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)

    if ($isAdmin) {
        # Работаем с системным PATH
        $oldPath = [Environment]::GetEnvironmentVariable("Path", [EnvironmentVariableTarget]::Machine)
        if ($oldPath -notlike "*$installPath*") {
            $newPath = "$oldPath;$installPath"
            [Environment]::SetEnvironmentVariable("Path", $newPath, [EnvironmentVariableTarget]::Machine)
            Write-Host "Sysinternals Suite added to SYSTEM PATH."
        } else {
            Write-Host "Sysinternals Suite already in SYSTEM PATH."
        }
    } else {
        # Работаем с PATH текущего пользователя
        $oldPathUser = [Environment]::GetEnvironmentVariable("Path", [EnvironmentVariableTarget]::User)
        if ($oldPathUser -notlike "*$installPath*") {
            $newPathUser = "$oldPathUser;$installPath"
            [Environment]::SetEnvironmentVariable("Path", $newPathUser, [EnvironmentVariableTarget]::User)
            Write-Host "Sysinternals Suite added to USER PATH."
        } else {
            Write-Host "Sysinternals Suite already in USER PATH."
        }
    }

    Write-Host "Installation complete."
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

### WIM ###
# Просмотр информации о WIM
function Get-WimInfo {
    param(
        [Parameter(Mandatory=$true)]
        [string]$WimFile
    )
    dism /Get-WimInfo /WimFile:$WimFile
}

# Монтирование WIM
function Mount-WimImage {
    param(
        [Parameter(Mandatory=$true)]
        [string]$WimFile,
        [Parameter(Mandatory=$true)]
        [string]$MountDir,
        [int]$Index = 1
    )
    if (-not (Test-Path $MountDir)) { New-Item -ItemType Directory -Path $MountDir }
    dism /Mount-Wim /WimFile:$WimFile /Index:$Index /MountDir:$MountDir
}

# Размонтирование WIM
function Dismount-WimImage {
    param(
        [Parameter(Mandatory=$true)]
        [string]$MountDir,
        [switch]$Commit
    )
    $option = if ($Commit) { "/Commit" } else { "/Discard" }
    dism /Unmount-Wim /MountDir:$MountDir $option
}

# Добавление драйвера в WIM
function Add-WimDriver {
    param(
        [Parameter(Mandatory=$true)]
        [string]$MountDir,
        [Parameter(Mandatory=$true)]
        [string]$DriverPath
    )
    dism /Add-Driver /Image:$MountDir /Driver:$DriverPath
}

# Добавление пакета в WIM
function Add-WimPackage {
    param(
        [Parameter(Mandatory=$true)]
        [string]$MountDir,
        [Parameter(Mandatory=$true)]
        [string]$PackagePath
    )
    dism /Add-Package /Image:$MountDir /PackagePath:$PackagePath
}

# Создание загрузочной флешки WinPE
function Create-WinPEUSB {
    param(
        [Parameter(Mandatory=$true)]
        [string]$WinPEPath,
        [Parameter(Mandatory=$true)]
        [string]$USBDrive
    )
    MakeWinPEMedia /UFD $WinPEPath $USBDrive
}

# Создание ISO WinPE
function Create-WinPEISO {
    param(
        [Parameter(Mandatory=$true)]
        [string]$WinPEPath,
        [Parameter(Mandatory=$true)]
        [string]$ISOPath
    )
    MakeWinPEMedia /ISO $WinPEPath $ISOPath
}



### CUSTOM ###
function wt {
	$modulePath = Join-Path $HOME "Documents\WindowsPowerShell\Modules"
	git clone https://github.com/zh54321/Get-WorkTime (Join-Path $modulePath "Get-WorkTime")
	# Import the module
	Import-Module Get-WorkTime
	Get-WorkTime | Format-Table -AutoSize -Wrap
}

Set-Alias np npp
Set-Alias psx Get-Process
Set-Alias ggl google