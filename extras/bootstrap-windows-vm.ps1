# Bootstrap a Windows Server 2022 VM for Extempore builds.
# Run in an elevated PowerShell session.

Set-ExecutionPolicy Bypass -Scope Process -Force
[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072

# Install Chocolatey if missing.
if (-not (Get-Command choco -ErrorAction SilentlyContinue)) {
  iex ((New-Object System.Net.WebClient).DownloadString('https://community.chocolatey.org/install.ps1'))
}

# Prefer the canonical choco path in Run Command where PATH is unreliable.
$ChocoExe = 'C:\ProgramData\chocolatey\bin\choco.exe'
if (-not (Test-Path $ChocoExe)) {
  $ChocoExe = (Get-Command choco -ErrorAction SilentlyContinue).Source
}

function Test-Tool($Command, $Paths) {
  if (Get-Command $Command -ErrorAction SilentlyContinue) { return $true }
  foreach ($path in $Paths) {
    if (Test-Path $path) { return $true }
  }
  return $false
}

# Core build tooling + Node LTS (install only what's missing).
if ($ChocoExe) {
  $packages = @()
  if (-not (Test-Tool 'git' @('C:\Program Files\Git\cmd\git.exe'))) { $packages += 'git' }
  if (-not (Test-Tool 'cmake' @('C:\Program Files\CMake\bin\cmake.exe'))) { $packages += 'cmake' }
  if (-not (Test-Tool 'ninja' @('C:\Program Files\Ninja\ninja.exe'))) { $packages += 'ninja' }
  if (-not (Test-Tool 'python' @('C:\Python311\python.exe','C:\Python310\python.exe'))) { $packages += 'python' }
  if (-not (Test-Tool '7z' @('C:\Program Files\7-Zip\7z.exe'))) { $packages += '7zip' }
  if (-not (Test-Tool 'node' @('C:\Program Files\nodejs\node.exe'))) { $packages += 'nodejs-lts' }

  if ($packages.Count -gt 0) {
    & $ChocoExe install -y @packages
  }

  $vcvars = 'C:\Program Files (x86)\Microsoft Visual Studio\2022\BuildTools\VC\Auxiliary\Build\vcvars64.bat'
  if (-not (Test-Path $vcvars)) {
    & $ChocoExe install -y visualstudio2022buildtools visualstudio2022-workload-vctools
  }
} else {
  throw "Chocolatey not found; install failed."
}

# Refresh PATH for this session.
if (Test-Path "$env:ChocolateyInstall\helpers\chocolateyProfile.psm1") {
  Import-Module "$env:ChocolateyInstall\helpers\chocolateyProfile.psm1"
  refreshenv
}

# Install Claude Code.
$npm = 'C:\Program Files\nodejs\npm.cmd'
if (Test-Path $npm) {
  & $npm install -g @anthropic-ai/claude-code
} else {
  npm install -g @anthropic-ai/claude-code
}

# Enable OpenSSH server and firewall rule.
Add-WindowsCapability -Online -Name OpenSSH.Server~~~~0.0.1.0
Start-Service sshd
Set-Service -Name sshd -StartupType 'Automatic'
if (-not (Get-NetFirewallRule -Name sshd -ErrorAction SilentlyContinue)) {
  New-NetFirewallRule -Name sshd -DisplayName 'OpenSSH Server (sshd)' -Enabled True -Direction Inbound -Protocol TCP -Action Allow -LocalPort 22
}

# Clone repo.
$RepoUrl = 'https://github.com/extemporelang/extempore.git'
$RepoRoot = 'C:\src'
if (-not (Test-Path $RepoRoot)) {
  New-Item -ItemType Directory -Path $RepoRoot | Out-Null
}
Set-Location $RepoRoot
if (-not (Test-Path (Join-Path $RepoRoot 'extempore'))) {
  $git = 'C:\Program Files\Git\cmd\git.exe'
  if (Test-Path $git) {
    & $git clone $RepoUrl
  } else {
    git clone $RepoUrl
  }
}

Write-Host 'Bootstrap complete. Next steps:'
Write-Host '1) claude auth login'
Write-Host '2) Open "x64 Native Tools Command Prompt for VS 2022"'
Write-Host '3) cd C:\src\extempore && mkdir build && cd build'
Write-Host '4) cmake .. -G Ninja -DCMAKE_BUILD_TYPE=Release'
Write-Host '5) cmake --build . -j 8'
