param(
  [switch]$AllowStaleData
)

$ErrorActionPreference = "Stop"

$siteRoot = Split-Path -Parent $PSScriptRoot
$rscript = "C:\Program Files\R\R-4.4.1\bin\Rscript.exe"
$quarto = "C:\Program Files\RStudio\resources\app\bin\quarto\bin\quarto.exe"
$quartoLocalData = Join-Path $siteRoot ".localappdata"
$workspaceRoot = Split-Path -Parent $siteRoot
$localRLibrary = Join-Path $workspaceRoot "sabrhoodR\.Rlib"

if (-not (Test-Path -LiteralPath $rscript)) { throw "Rscript was not found at $rscript" }
if (-not (Test-Path -LiteralPath $quarto)) { throw "Quarto was not found at $quarto" }

New-Item -ItemType Directory -Force -Path $quartoLocalData | Out-Null
$env:LOCALAPPDATA = $quartoLocalData
$env:LANG = "C"
$env:LC_ALL = "C"
$env:LC_CTYPE = "C"
if (Test-Path -LiteralPath $localRLibrary) {
  $env:R_LIBS_USER = $localRLibrary
}

Push-Location $siteRoot
try {
  $validationArgs = @("scripts/validate_site.R")
  if ($AllowStaleData) { $validationArgs += "--allow-stale-data" }

  & $rscript --vanilla scripts/build_site_fragments.R
  if ($LASTEXITCODE -ne 0) { throw "Fragment generation failed." }

  & $rscript --vanilla @validationArgs
  if ($LASTEXITCODE -ne 0) { throw "Source validation failed." }

  # A cancelled Quarto run can leave intermediate HTML and dependency folders
  # beside QMD sources. Quarto's next project scan can encounter a sidecar while
  # its paired dependency directory is disappearing and fail before rendering.
  $teamReportSource = Join-Path $siteRoot "team-reports"
  $qmdSourceDirectories = @(
    $siteRoot,
    (Join-Path $siteRoot "articles"),
    (Join-Path $siteRoot "posts"),
    $teamReportSource
  )
  $qmdSources = foreach ($sourceDirectory in $qmdSourceDirectories) {
    if (Test-Path -LiteralPath $sourceDirectory) {
      Get-ChildItem -LiteralPath $sourceDirectory -File -Filter "*.qmd"
    }
  }
  $intermediatePaths = foreach ($qmdSource in $qmdSources) {
    $stem = [IO.Path]::GetFileNameWithoutExtension($qmdSource.Name)
    Join-Path $qmdSource.DirectoryName ($stem + ".html")
    Join-Path $qmdSource.DirectoryName ($stem + "_files")
  }
  $intermediatePaths = @($intermediatePaths | Where-Object { Test-Path -LiteralPath $_ } | Select-Object -Unique)
  foreach ($intermediatePath in $intermediatePaths) {
    $resolvedIntermediate = (Resolve-Path -LiteralPath $intermediatePath).Path
    if (-not $resolvedIntermediate.StartsWith(
          $siteRoot + [IO.Path]::DirectorySeparatorChar,
          [StringComparison]::OrdinalIgnoreCase
        )) {
      throw "Refusing to remove an intermediate outside the site root: $resolvedIntermediate"
    }
    Remove-Item -LiteralPath $resolvedIntermediate -Recurse -Force
  }
  if ($intermediatePaths.Count -gt 0) {
    Write-Host "Cleared $($intermediatePaths.Count) interrupted Quarto render artifacts."
  }

  # Source-side intermediates were removed explicitly above. Start-Process
  # waits for Quarto's full Deno process tree; a direct PowerShell invocation
  # can return while rendered files are still being moved into docs.
  # Some desktop shells expose both Path and PATH; Start-Process treats those
  # as a duplicate dictionary key, so normalize to the Windows spelling first.
  $processPath = [Environment]::GetEnvironmentVariable("Path", "Process")
  [Environment]::SetEnvironmentVariable("PATH", $null, "Process")
  [Environment]::SetEnvironmentVariable("Path", $processPath, "Process")
  $quartoProcess = Start-Process `
    -FilePath $quarto `
    -ArgumentList @("render", "--no-clean") `
    -WorkingDirectory $siteRoot `
    -NoNewWindow `
    -Wait `
    -PassThru
  if ($quartoProcess.ExitCode -ne 0) { throw "Core Quarto render failed." }

  $completedIntermediatePaths = foreach ($qmdSource in $qmdSources) {
    $stem = [IO.Path]::GetFileNameWithoutExtension($qmdSource.Name)
    Join-Path $qmdSource.DirectoryName ($stem + ".html")
    Join-Path $qmdSource.DirectoryName ($stem + "_files")
  }
  $completedIntermediatePaths = @(
    $completedIntermediatePaths |
      Where-Object { Test-Path -LiteralPath $_ } |
      Select-Object -Unique
  )
  foreach ($completedIntermediatePath in $completedIntermediatePaths) {
    $resolvedIntermediate = (Resolve-Path -LiteralPath $completedIntermediatePath).Path
    if (-not $resolvedIntermediate.StartsWith(
          $siteRoot + [IO.Path]::DirectorySeparatorChar,
          [StringComparison]::OrdinalIgnoreCase
        )) {
      throw "Refusing to remove a completed intermediate outside the site root: $resolvedIntermediate"
    }
    Remove-Item -LiteralPath $resolvedIntermediate -Recurse -Force
  }
  if ($completedIntermediatePaths.Count -gt 0) {
    Write-Host "Cleared $($completedIntermediatePaths.Count) completed Quarto render artifacts."
  }

  $finalizeArgs = @("scripts/finalize_rendered_site.R")
  if ($AllowStaleData) { $finalizeArgs += "--allow-stale-data" }
  & $rscript --vanilla @finalizeArgs
  if ($LASTEXITCODE -ne 0) { throw "Rendered-site finalization failed." }

  Write-Host "Site build complete: $siteRoot\docs\index.html"
}
finally {
  Pop-Location
}
