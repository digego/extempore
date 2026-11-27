# Install Extempore for the current user, on Windows. In PowerShell:
#
#   irm https://extemporelang.github.io/install.ps1 | iex
#
# This downloads the latest release, checks it against the release's
# SHA256SUMS and unpacks it into %LOCALAPPDATA%\Programs\Extempore\VERSION.
# The junction %LOCALAPPDATA%\Programs\Extempore\current then points at that
# version, and current\bin goes on your user PATH. Run it again to upgrade;
# earlier versions stay where they are until you delete them.
#
# Environment variables:
#   EXTEMPORE_VERSION         install this release (e.g. v0.13.0), not the latest
#   EXTEMPORE_NO_MODIFY_PATH  set to leave your user PATH alone
#   EXTEMPORE_DOWNLOAD_URL    where the archives and SHA256SUMS are (for testing)

& {
    $ErrorActionPreference = 'Stop'
    # Invoke-WebRequest's progress bar slows Windows PowerShell's downloads
    # many times over
    $ProgressPreference = 'SilentlyContinue'
    [Net.ServicePointManager]::SecurityProtocol = [Net.ServicePointManager]::SecurityProtocol -bor [Net.SecurityProtocolType]::Tls12

    # Windows on Arm runs the x86_64 release under emulation
    $platform = 'windows-x86_64'
    $releases = 'https://github.com/digego/extempore/releases'
    $base = if ($env:EXTEMPORE_DOWNLOAD_URL) {
        $env:EXTEMPORE_DOWNLOAD_URL
    } elseif ($env:EXTEMPORE_VERSION) {
        "$releases/download/$($env:EXTEMPORE_VERSION)"
    } else {
        "$releases/latest/download"
    }

    $tmp = Join-Path ([IO.Path]::GetTempPath()) ("extempore-" + [guid]::NewGuid())
    New-Item -ItemType Directory -Path $tmp | Out-Null
    try {
        Invoke-WebRequest -UseBasicParsing -Uri "$base/SHA256SUMS" -OutFile "$tmp\SHA256SUMS"
        $entry = Get-Content "$tmp\SHA256SUMS" |
            ForEach-Object { if ($_ -match "^([0-9a-fA-F]{64}) +\*?(extempore-$platform-(.+)\.zip)$") { $Matches } } |
            Select-Object -First 1
        if (-not $entry) { throw "this release has no download for $platform" }
        $archive = $entry[2]
        $version = $entry[3]

        Write-Host "Downloading Extempore $version for $platform"
        Invoke-WebRequest -UseBasicParsing -Uri "$base/$archive" -OutFile "$tmp\$archive"
        if ((Get-FileHash -Algorithm SHA256 "$tmp\$archive").Hash -ne $entry[1]) {
            throw "$archive doesn't match its checksum in SHA256SUMS"
        }
        Add-Type -AssemblyName System.IO.Compression.FileSystem
        [IO.Compression.ZipFile]::ExtractToDirectory("$tmp\$archive", "$tmp\unpacked")
        $tree = Join-Path "$tmp\unpacked" ($archive -replace '\.zip$', '')
        if (-not (Test-Path "$tree\bin\extempore.exe")) { throw "$archive doesn't hold bin\extempore.exe" }

        $root = Join-Path $env:LOCALAPPDATA 'Programs\Extempore'
        $dest = Join-Path $root $version
        $current = Join-Path $root 'current'
        New-Item -ItemType Directory -Force -Path $root | Out-Null
        if (Test-Path $dest) { Remove-Item -Recurse -Force $dest }
        Move-Item $tree $dest
        Get-ChildItem -Recurse -File $dest | Unblock-File
        # rmdir removes the junction and leaves the version it points at
        if (Test-Path $current) { cmd /c rmdir "$current" | Out-Null }
        New-Item -ItemType Junction -Path $current -Target $dest | Out-Null

        $bin = Join-Path $current 'bin'
        & "$bin\extempore.exe" --version | Out-Null
        if ($LASTEXITCODE -ne 0) { throw "the installed $bin\extempore.exe doesn't run" }
        Write-Host "Installed Extempore $version in $dest"

        $userPath = [Environment]::GetEnvironmentVariable('Path', 'User')
        $onPath = ($userPath -split ';') -contains $bin
        if (-not $onPath -and -not $env:EXTEMPORE_NO_MODIFY_PATH) {
            $newPath = if ($userPath) { "$userPath;$bin" } else { $bin }
            [Environment]::SetEnvironmentVariable('Path', $newPath, 'User')
            Write-Host "Added $bin to your user PATH. Open a new terminal (and restart VS Code) to use extempore there."
        } elseif (-not $onPath) {
            Write-Host "$bin isn't on your PATH: add it, or run $bin\extempore.exe"
        }
        $env:Path = "$bin;$env:Path"
        Write-Host "Run extempore to start it."
    } finally {
        Remove-Item -Recurse -Force $tmp -ErrorAction SilentlyContinue
    }
}
