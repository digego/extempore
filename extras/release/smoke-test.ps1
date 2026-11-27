# Install a release archive with docs/public/install.ps1, as a user would, and
# check the installed extempore runs from outside the tree. Then run the
# archive in a clean Windows Server Core container, which has no Visual C++
# runtime: as unpacked it should run, and without the runtime DLLs it ships in
# bin\ it shouldn't (or the container proves nothing).
#
# Usage: extras/release/smoke-test.ps1 DIST

param([Parameter(Mandatory)] [string] $Dist)

$ErrorActionPreference = 'Stop'
$Dist = (Resolve-Path $Dist).Path
$repo = (Resolve-Path "$PSScriptRoot\..\..").Path
$scratch = Join-Path ([IO.Path]::GetTempPath()) ("xtm-smoke-" + [guid]::NewGuid())
New-Item -ItemType Directory -Path "$scratch\work", "$scratch\localappdata" | Out-Null

function Check($cond, $what) {
    if (-not $cond) { throw "smoke test failed: $what" }
}

$port = 8765
$server = Start-Process -PassThru -NoNewWindow python `
    -ArgumentList '-m', 'http.server', '--bind', '127.0.0.1', '--directory', $Dist, $port
try {
    foreach ($i in 1..50) {
        try { Invoke-WebRequest -UseBasicParsing "http://127.0.0.1:$port/SHA256SUMS" | Out-Null; break }
        catch { Start-Sleep -Milliseconds 200 }
    }

    # the installer, run the way the docs say: piped into Invoke-Expression
    $env:LOCALAPPDATA = "$scratch\localappdata"
    $env:EXTEMPORE_DOWNLOAD_URL = "http://127.0.0.1:$port"
    Get-Content -Raw "$repo\docs\public\install.ps1" | Invoke-Expression
    $bin = "$scratch\localappdata\Programs\Extempore\current\bin"
    Check (([Environment]::GetEnvironmentVariable('Path', 'User') -split ';') -contains $bin) "user PATH has $bin"

    Push-Location "$scratch\work"
    $out = & "$bin\extempore.exe" exec -e '(sys:load "libs/core/math.xtm") (println (sqrt 2.0))' | Out-String
    Pop-Location
    Write-Host $out
    Check ($LASTEXITCODE -eq 0) "the installed extempore.exe runs"
    Check ($out -match '1\.414214') "the installed extempore.exe computes"

    # the archive, unpacked, in a container without the Visual C++ runtime
    $zip = Get-ChildItem "$Dist\*.zip" | Select-Object -First 1
    Add-Type -AssemblyName System.IO.Compression.FileSystem
    [IO.Compression.ZipFile]::ExtractToDirectory($zip.FullName, "$scratch\clean")
    $tree = (Get-ChildItem "$scratch\clean" | Select-Object -First 1).FullName
    Set-Content -Path "$tree\smoke.xtm" -Value '(sys:load "libs/core/math.xtm") (println (sqrt 2.0))'
    Copy-Item -Recurse $tree "$scratch\nocrt"
    $runtime = Get-ChildItem "$scratch\nocrt\bin" -Include 'vcruntime*.dll', 'msvcp*.dll', 'concrt*.dll' -Recurse
    Check ($runtime.Count -gt 0) "bin\ holds the Visual C++ runtime DLLs"
    $runtime | Remove-Item

    $image = 'mcr.microsoft.com/windows/servercore:ltsc2025'
    docker pull -q $image
    $out = docker run --rm -v "${tree}:C:\xtm" $image C:\xtm\bin\extempore.exe exec C:\xtm\smoke.xtm | Out-String
    Write-Host $out
    Check ($LASTEXITCODE -eq 0 -and $out -match '1\.414214') "the archive runs in a clean container"
    docker run --rm -v "$scratch\nocrt:C:\xtm" $image C:\xtm\bin\extempore.exe --version
    $without = $LASTEXITCODE
    Write-Host "without the runtime DLLs: exit status $without"
    Check ($without -ne 0) "the container lacks the Visual C++ runtime (else this test proves nothing)"
    Write-Host "smoke test passed"
} finally {
    Stop-Process -Id $server.Id -ErrorAction SilentlyContinue
}
