Import-Module posh-git

$PSDefaultParameterValues['Out-File:Encoding'] = 'utf8'

# use vi mode
Set-PSReadlineOption -EditMode vi -BellStyle None
# keep or reset to powershell default
Set-PSReadlineKeyHandler -Key Shift+Tab -Function TabCompletePrevious
# define Ctrl+Tab like default Tab behavior
Set-PSReadlineKeyHandler -Key Ctrl+Tab -Function TabCompleteNext
# define Tab like bash
Set-PSReadlineKeyHandler -Key Tab -Function Complete
# history completion
Set-PSReadLineOption -HistorySearchCursorMovesToEnd
Set-PSReadlineKeyHandler -Key UpArrow -Function HistorySearchBackward
Set-PSReadlineKeyHandler -Key DownArrow -Function HistorySearchForward

Remove-PSReadLineKeyHandler 'Ctrl+r'
Remove-PSReadLineKeyHandler 'Ctrl+t'

Import-Module PSFzf

# scoop bucket add scoop-completion https://github.com/Moeologist/scoop-completion
# scoop install scoop-completion
# scoop config alias @{}
Import-Module "$($(Get-Item $(Get-Command scoop).Path).Directory.Parent.FullName)\modules\scoop-completion"

Import-Module npm-completion

# Command completter for the winget package manager.
Register-ArgumentCompleter -Native -CommandName winget -ScriptBlock {
  param($wordToComplete, $commandAst, $cursorPosition)
    [Console]::InputEncoding = [Console]::OutputEncoding = $OutputEncoding = [System.Text.Utf8Encoding]::new()
    $Local:word = $wordToComplete.Replace('"', '""')
      $Local:ast = $commandAst.ToString().Replace('"', '""')
      winget complete --word="$Local:word" --commandline "$Local:ast" --position $cursorPosition | ForEach-Object {
        [System.Management.Automation.CompletionResult]::new($_, $_, 'ParameterValue', $_)
      }
}

function Get-Youtube-Music($url) { youtube-dl --extract-audio --audio-format vorbis --audio-quality 3 --output "%(title)s.%(ext)s" $url }

function Get-FolderSize {
  [CmdletBinding()]
    Param (
        [Parameter(Mandatory=$true,ValueFromPipeline=$true)]
        $Path
        )
      if ( (Test-Path $Path) -and (Get-Item $Path).PSIsContainer ) {
        $Measure = Get-ChildItem $Path -Recurse -Force -ErrorAction SilentlyContinue | Measure-Object -Property Length -Sum
          $Sum = '{0:N2}' -f ($Measure.Sum / 1Gb)
          [PSCustomObject]@{
            "Path" = $Path
              "Size($Gb)" = $Sum
          }
      }
}

function du { Get-FolderSize($args) }

Remove-Alias ls
function ls { lsd --ignore-glob="ntuser.*" --ignore-glob="NTUSER.*" --ignore-glob="System Volume Information" $args }

Remove-Alias cat
function cat { bat $args }

function Select-Git-Branch { git checkout @(git branch | fzf | ForEach-Object { $_.Trim() }) }

# https://github.com/starship/starship
Invoke-Expression (&starship init powershell)

