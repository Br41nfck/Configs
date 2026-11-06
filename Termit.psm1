# Fish-like git applet for Windows Terminal
# Location: ..\Documents\PowerShell\Modules\Termit\
$script:Abbr = @{
    'g.'  = 'git add .'
    'ga'  = 'git add '
    'gs'  = 'git status -sb'
    'gcm' = 'git commit -m "%"'
    'gp'  = 'git push'
    'gpl' = 'git pull'
    'gl'  = 'git log --oneline --graph --decorate -20'
    'gco' = 'git checkout '
    'gd'  = 'git diff'
}

Set-PSReadLineOption -PredictionSource History
Set-PSReadLineOption -PredictionViewStyle InlineView

Set-PSReadLineKeyHandler -Key Tab -ScriptBlock {
    $line = $null; $cursor = $null
    [Microsoft.PowerShell.PSConsoleReadLine]::GetBufferState([ref]$line, [ref]$cursor)
    $left = $line.Substring(0, $cursor)

    $m = [regex]::Match($left, '^\s*(\S+)$')
    if ($m.Success -and $script:Abbr.ContainsKey($m.Groups[1].Value)) {
        $g   = $m.Groups[1]
        $exp = $script:Abbr[$g.Value]
        $pos = $exp.IndexOf('%')
        if ($pos -lt 0) { $pos = $exp.Length }
        $exp = $exp.Replace('%', '')
        [Microsoft.PowerShell.PSConsoleReadLine]::Replace($g.Index, $g.Length, $exp)
        [Microsoft.PowerShell.PSConsoleReadLine]::SetCursorPosition($g.Index + $pos)
    } else {
        [Microsoft.PowerShell.PSConsoleReadLine]::TabCompleteNext()
    }
}