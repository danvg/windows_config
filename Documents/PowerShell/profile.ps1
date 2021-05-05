Import-Module posh-git

$PSDefaultParameterValues['Out-File:Encoding'] = 'utf8'

Set-PSReadlineOption -EditMode vi -BellStyle None
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

function Get-Youtube-Music($url) { youtube-dl --extract-audio --audio-format vorbis --audio-quality 3 --output "%(title)s.%(ext)s" $url }

Remove-Alias ls
function ls { lsd --ignore-glob="ntuser.*" --ignore-glob="NTUSER.*" --ignore-glob="System Volume Information" $args }

Remove-Alias cat
function cat { bat $args }

# https://github.com/starship/starship
Invoke-Expression (&starship init powershell)
