# Windows 10 Config

## Regular installers

[Latest Powershell](https://github.com/PowerShell/Powershell)  
[AMD Drivers](https://www.amd.com/en/support)  
[7-Zip](https://www.7-zip.org/)  
[GNAT Community](https://www.adacore.com/download)  
[Ada Language Server](https://github.com/AdaCore/ada_language_server)  
[Steam](https://store.steampowered.com/)  
[Epic games store](https://www.epicgames.com/store/en-US/)

## Scoop

Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser  
iwr -useb get.scoop.sh | iex  
scoop config alias @{}

scoop bucket add extras  
scoop bucket add java  
scoop bucket add nonportable  
scoop bucket add scoop-completion https://github.com/Moeologist/scoop-completion

scoop install
ag
bat
cmake
ctags
dia
discord
dos2unix
doxygen
ds4windows
fd
ffmpeg
filezilla
fzf
git-with-openssh
googlechrome
heroku-cli
imagemagick
kdiff3
lazygit
lf
libreoffice-fresh
llvm
lsd
mongodb
mongodb-compass-community
neofetch
neovim-nightly
ninja
nodejs
nvidia-display-driver-np
obs-studio
openjdk
powertoys
python
qbittorrent-portable
rainmeter
ripgrep
rufus
scoop-completion
speedcrunch
starship
sudo
sumatrapdf
windows-terminal
yarn
youtube-dl
zoom

## Postinstall PowerShell

Install-Module posh-git -Scope CurrentUser  
Install-Module oh-my-posh -Scope CurrentUser  
Install-Module -Name PSFzf

## Postinstall NPM

npm install --user
csslint
eslint
jshint
@angular/language-server
bash-language-server
csslint
emmet-ls
eslint
jshint
neovim
pyright
tslib
typescript-language-server
vim-language-server
vscode-css-languageserver-bin
vscode-html-languageserver-bin
vscode-json-languageserver-bin

## Postinstall Pip

pip install --user
wheel
pynvim
cpplint
cmake-language-server
