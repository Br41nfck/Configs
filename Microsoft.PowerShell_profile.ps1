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

function iperf {
    Start-Process "D:\SOFT\iperf\iperf3.exe" -ArgumentList "-s" -WindowStyle Hidden
    Write-Host "iperf was started in SILENT mode"
}

function npp {
	$pathX86 = "C:\Program Files (x86)\Notepad++\notepad++.exe"
	$pathX64 = "C:\Program Files\Notepad++\notepad++.exe"
	
	if (Test-Path -Path $pathX86 -PathType Leaf) {
		& $pathX86 @args
	} elseif (Test-Path -Path $pathX64 -PathType Leaf) {
		& $pathX64 @args
	}
	else {
		Write-Host "File: $pathX64 or $pathX86 not found!"
	}
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

## NSSM 
#### NSSM STATUS
function nssmst {
	Get-CimInstance win32_service | Where-Object {$_.PathName -like '*nssm*'} | Select-Object Name, DisplayName, State, PathName
}

#### NSSM RESTART
function nssmrst {
    & "nssm.exe" restart $args
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
	Test-Connection 192.168.50.1  -Count 4
	Test-Connection 192.168.10.18 -Count 4
}

# Update PowerShell
function updps {
	winget upgrade Microsoft.PowerShell
}

function verps {
	$PSVersionTable.PSVersion
}

function ll {
    Get-ChildItem -Force
}

function la {
    Get-ChildItem -Force -Recurse
}

function ai {
    param(
        [string]$prompt,
        [string]$model = "gpt-4o"
    )
    
    $headers = @{
        "Authorization" = "Bearer $env:GITHUB_TOKEN"
        "Content-Type" = "application/json"
    }
    
    $body = @{
        model = $model
        messages = @(
            @{
                role = "user"
                content = $prompt
            }
        )
        max_tokens = 1000
    } | ConvertTo-Json -Depth 10
    
    try {
        $response = Invoke-RestMethod -Method Post -Uri "https://models.inference.ai.azure.com/chat/completions" -Headers $headers -Body $body
        return $response.choices[0].message.content
    } catch {
        return "❌ Ошибка: $($_.Exception.Message)"
    }
}

function ai-ru {
    param(
        [string]$prompt,
        [string]$model = "gpt-4o"
    )
    
    $headers = @{
        "Authorization" = "Bearer $env:GITHUB_TOKEN"
        "Content-Type" = "application/json"
    }
    
    $body = @{
        model = $model
        messages = @(
            @{
                role = "user"
                content = $prompt
            }
        )
        max_tokens = 1000
    } | ConvertTo-Json -Depth 10
    
    try {
        $response = Invoke-RestMethod -Method Post -Uri "https://models.inference.ai.azure.com/chat/completions" -Headers $headers -Body $body
        $content = $response.choices[0].message.content
        
        # Конвертируем в правильную кодировку
        $bytes = [System.Text.Encoding]::UTF8.GetBytes($content)
        return [System.Text.Encoding]::UTF8.GetString($bytes)
    } catch {
        return "❌ Ошибка: $($_.Exception.Message)"
    }
}

# Также создаем функцию для кода с поддержкой русского
function ai-code-ru {
    param(
        [string]$prompt
    )
    
    $headers = @{
        "Authorization" = "Bearer $env:GITHUB_TOKEN"
        "Content-Type" = "application/json"
    }
    
    $body = @{
        model = "gpt-4o"
        messages = @(
            @{
                role = "user"
                content = "$prompt. Return only the code without any explanations or markdown formatting."
            }
        )
        max_tokens = 1000
    } | ConvertTo-Json -Depth 10
    
    try {
        $response = Invoke-RestMethod -Method Post -Uri "https://models.inference.ai.azure.com/chat/completions" -Headers $headers -Body $body
        $content = $response.choices[0].message.content
        $content = $content -replace '```\w*\n?', '' -replace '```', ''
        
        $bytes = [System.Text.Encoding]::UTF8.GetBytes($content)
        return [System.Text.Encoding]::UTF8.GetString($bytes)
    } catch {
        return "Ошибка: $($_.Exception.Message)"
    }
}

function ask {
    python C:\Users\Ilya\ai.py $args
}

function Test-ScriptSyntax {
    [CmdletBinding()]
    param (
        [Parameter(Mandatory = $true, ValueFromPipeline = $true, Position = 0)]
        [string]$Path
    )

    process {
        $ResolvedPath = Resolve-Path $Path -ErrorAction SilentlyContinue
        if (-not $ResolvedPath) {
            Write-Error "File not found at path: $Path"
            return
        }

        $parseErrors = $null
        [System.Management.Automation.Language.Parser]::ParseFile($ResolvedPath.Path, [ref]$null, [ref]$parseErrors)

        if ($parseErrors) {
            Write-Host "Syntax errors found in file: $Path" -ForegroundColor Red
            $parseErrors | ForEach-Object {
                Write-Host "- Line ($($_.Extent.StartLineNumber)), Column ($($_.Extent.StartColumnNumber)): $($_.Message)" -ForegroundColor Yellow
            }
        } else {
            Write-Host "No syntax errors found: $Path" -ForegroundColor Green
        }
    }
}

function install_packages {
	winget install --id -q Git.Git 7zip.7zip Microsoft.VisualStudioCode Microsoft.PowerShell Microsoft.PowerToys Docker.DockerDesktop Postman.Postman Python.Python.3.14 Mozilla.Firefox VideoLAN.VLC
}

Set-Alias np npp
Set-Alias psx Get-Process
Set-Alias ggl google