# powershell.exe -ExecutionPolicy ByPass -File .\build_neovim.ps1

$rootDir = $PWD.Path
$unzipDir = $rootDir + "\neovim-nightly-win64"
$nvimDir = $unzipDir + "\neovim-nightly"
$installDir = $env:USERPROFILE + "\neovim"

$cmakeExe = $env:USERPROFILE + "\scoop\apps\cmake\current\bin\cmake.exe"
$gitExe = $env:USERPROFILE + "\scoop\apps\git-with-openssh\current\bin\git.exe"

$downloadURL = "https://github.com/neovim/neovim/archive/nightly.zip"
$downloadedFile = $rootDir + "\neovim-nightly.zip"

if (-Not [System.IO.Directory]::Exists($rootDir)) {
    Write-Host "Error: $($rootDir) does not exist"
    Exit
}

if (-Not [System.IO.File]::Exists($downloadedFile)) {
    $webClient = New-Object System.Net.WebClient
    $webClient.DownloadFile($downloadURL, $downloadedFile)
} else {
    Write-Host "Warning: Using existing downloaded file $($downloadedFile)" 
}

if (-Not [System.IO.Directory]::Exists($unzipDir)) {
    mkdir $unzipDir
    Microsoft.PowerShell.Archive\Expand-Archive -LiteralPath $downloadedFile -DestinationPath $unzipDir
} else {
    Write-Host "Warning: Using existing unzipped dir $($unzipDir)"
}

if (-Not [System.IO.Directory]::Exists($nvimDir)) {
    Write-Host "Error: Unexpected unzipped results, expected $($nvimDir)"
    Exit
}

Set-Location $nvimDir

if (-Not [System.IO.Directory]::Exists("$($nvimDir)\.deps")) {
    mkdir "$($nvimDir)\.deps"
} else {
    Write-Host "Warning: Using existing $($nvimDir)\.deps directory"
}

& $cmakeExe -D CMAKE_BUILD_TYPE=Release -G "Visual Studio 16 2019" -A x64 -S .\third-party -B .deps -D GIT_EXECUTABLE=$gitExe
& $cmakeExe --build .deps --config Release

if (-Not [System.IO.Directory]::Exists("$($nvimDir)\build")) {
    mkdir "$($nvimDir)\build"
} else {
    Write-Host "Warning: Using existing $($nvimDir)\build directory"
}

& $cmakeExe -D CMAKE_BUILD_TYPE=Release -G "Visual Studio 16 2019" -A x64 -D CMAKE_INSTALL_PREFIX=$installDir -S . -B build
& $cmakeExe --build build --config Release

if ([System.IO.Directory]::Exists($installDir)) {
    Write-Host "Warning: Moving existing neovim install dir to $($installDir).old directory"
    Move-Item $installDir "$($installDir).old"
}

& $cmakeExe --install build

Write-Host "Neovim installed in $($installDir)"