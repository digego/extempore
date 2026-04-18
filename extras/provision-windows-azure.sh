#!/usr/bin/env bash
set -euo pipefail

# Provision a Windows Server 2022 VM on Azure and enable SSH with your public key.
# Requires: az CLI authenticated with sufficient permissions.
# Usage:
#   SUBSCRIPTION_ID="..." LOCATION="australiaeast" ./provision-windows-azure.sh setup
#   SUBSCRIPTION_ID="..." ./provision-windows-azure.sh restart
#   SUBSCRIPTION_ID="..." ./provision-windows-azure.sh stop
#   SUBSCRIPTION_ID="..." ./provision-windows-azure.sh destroy
# LOCATION defaults to australiaeast.

SUBSCRIPTION_ID="${SUBSCRIPTION_ID:-}"
RESOURCE_GROUP="${RESOURCE_GROUP:-extempore-win}"
LOCATION="${LOCATION:-australiaeast}"
VM_NAME="${VM_NAME:-extempore-winvm}"
VM_SIZE="${VM_SIZE:-Standard_D4s_v5}"
ADMIN_USER="${ADMIN_USER:-azureuser}"
SSH_PUBKEY_PATH="${SSH_PUBKEY_PATH:-$HOME/.ssh/id_ed25519.pub}"

ACTION="${1:-setup}"

if [ -z "$SUBSCRIPTION_ID" ]; then
  echo "SUBSCRIPTION_ID is required. Export it before running." >&2
  exit 1
fi

if [ "$ACTION" = "setup" ] && [ ! -f "$SSH_PUBKEY_PATH" ]; then
  echo "SSH public key not found at $SSH_PUBKEY_PATH" >&2
  exit 1
fi

if ! command -v az >/dev/null 2>&1; then
  echo "az CLI not found. Install Azure CLI first." >&2
  exit 1
fi

az account set --subscription "$SUBSCRIPTION_ID"

case "$ACTION" in
  setup)
    read -r -s -p "Admin password for $ADMIN_USER (will be used only for VM creation): " ADMIN_PASSWORD
    printf "\n"

    if [ -z "$ADMIN_PASSWORD" ]; then
      echo "Password cannot be empty." >&2
      exit 1
    fi

    # Create resource group if needed.
    az group create -n "$RESOURCE_GROUP" -l "$LOCATION" >/dev/null

    # Create the VM with password auth (SSH for Windows is enabled post-provisioning).
    if az vm show -g "$RESOURCE_GROUP" -n "$VM_NAME" >/dev/null 2>&1; then
      echo "VM $VM_NAME already exists; skipping create."
    else
      az vm create \
        -g "$RESOURCE_GROUP" \
        -n "$VM_NAME" \
        --image MicrosoftWindowsServer:WindowsServer:2022-datacenter-g2:latest \
        --size "$VM_SIZE" \
        --admin-username "$ADMIN_USER" \
        --admin-password "$ADMIN_PASSWORD" \
        --public-ip-sku Standard
    fi

    # Open SSH port.
    az vm open-port -g "$RESOURCE_GROUP" -n "$VM_NAME" --port 22 >/dev/null

    # Install and enable OpenSSH server, set firewall, and add the authorized key.
    PUBKEY_CONTENT=$(cat "$SSH_PUBKEY_PATH")

    PS_TEMPLATE=$(cat <<'PS1'
Add-WindowsCapability -Online -Name OpenSSH.Server~~~~0.0.1.0
Start-Service sshd
Set-Service -Name sshd -StartupType 'Automatic'
if (-not (Get-NetFirewallRule -Name sshd -ErrorAction SilentlyContinue)) { New-NetFirewallRule -Name sshd -DisplayName 'OpenSSH Server (sshd)' -Enabled True -Direction Inbound -Protocol TCP -Action Allow -LocalPort 22 }
$pubkey = @'
__PUBKEY_CONTENT__
'@
# For admin users, OpenSSH on Windows uses administrators_authorized_keys
New-Item -ItemType Directory -Force -Path C:\ProgramData\ssh | Out-Null
Set-Content -Path C:\ProgramData\ssh\administrators_authorized_keys -Value $pubkey
icacls C:\ProgramData\ssh\administrators_authorized_keys /inheritance:r /grant "SYSTEM:(F)" /grant "Administrators:(F)" | Out-Null
# Also set up user .ssh directory for the SSH private key (for git access)
New-Item -ItemType Directory -Force -Path C:\Users\__ADMIN_USER__\.ssh | Out-Null
# Install Git
if (-not (Get-Command git -ErrorAction SilentlyContinue)) {
  $gitInstaller = "$env:TEMP\Git-Installer.exe"
  [Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12
  $release = Invoke-RestMethod -Headers @{ 'User-Agent'='curl' } https://api.github.com/repos/git-for-windows/git/releases/latest
  $gitUrl = ($release.assets | Where-Object { $_.name -like '*64-bit.exe' } | Select-Object -First 1 -ExpandProperty browser_download_url)
  curl.exe -L -o $gitInstaller $gitUrl
  Start-Process -FilePath $gitInstaller -ArgumentList "/VERYSILENT","/NORESTART","/NOCANCEL","/SP-" -Wait
  $machinePath = [Environment]::GetEnvironmentVariable("Path", "Machine")
  if ($machinePath -notlike "*Git\cmd*") {
    [Environment]::SetEnvironmentVariable("Path", "$machinePath;C:\Program Files\Git\cmd", "Machine")
  }
}
$env:Path = [Environment]::GetEnvironmentVariable("Path", "Machine") + ";" + [Environment]::GetEnvironmentVariable("Path", "User")
if (Get-Command git -ErrorAction SilentlyContinue) { git config --global credential.helper manager-core }

# Install Node.js
if (-not (Get-Command node -ErrorAction SilentlyContinue)) {
  $nodeInstaller = "$env:TEMP\node-installer.msi"
  curl.exe -L -o $nodeInstaller "https://nodejs.org/dist/v22.12.0/node-v22.12.0-x64.msi"
  Start-Process msiexec.exe -ArgumentList "/i", $nodeInstaller, "/quiet", "/norestart" -Wait
  $machinePath = [Environment]::GetEnvironmentVariable("Path", "Machine")
  if ($machinePath -notlike "*nodejs*") {
    [Environment]::SetEnvironmentVariable("Path", "$machinePath;C:\Program Files\nodejs", "Machine")
  }
}
$env:Path = [Environment]::GetEnvironmentVariable("Path", "Machine") + ";" + [Environment]::GetEnvironmentVariable("Path", "User")

# Install Claude Code
if (Get-Command npm -ErrorAction SilentlyContinue) {
  npm install -g @anthropic-ai/claude-code
}
PS1
)

    PS_SCRIPT=${PS_TEMPLATE//__ADMIN_USER__/$ADMIN_USER}
    PS_SCRIPT=${PS_SCRIPT//__PUBKEY_CONTENT__/$PUBKEY_CONTENT}

    az vm run-command invoke \
      -g "$RESOURCE_GROUP" \
      -n "$VM_NAME" \
      --command-id RunPowerShellScript \
      --scripts "$PS_SCRIPT"

    PUBLIC_IP=$(az vm show -d -g "$RESOURCE_GROUP" -n "$VM_NAME" --query publicIps -o tsv)

    echo "VM created. SSH in with:"
    echo "ssh $ADMIN_USER@$PUBLIC_IP"
    ;;
  restart)
    az vm restart -g "$RESOURCE_GROUP" -n "$VM_NAME"
    ;;
  stop)
    az vm deallocate -g "$RESOURCE_GROUP" -n "$VM_NAME"
    ;;
  destroy)
    az group delete -n "$RESOURCE_GROUP" --yes
    ;;
  *)
    echo "Unknown action: $ACTION" >&2
    echo "Valid actions: setup, restart, stop, destroy" >&2
    exit 1
    ;;
esac
