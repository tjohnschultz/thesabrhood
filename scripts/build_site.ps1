$ErrorActionPreference = "Stop"

$siteRoot = Split-Path -Parent $PSScriptRoot
$rscript = "C:\Program Files\R\R-4.4.1\bin\Rscript.exe"
$quarto = "C:\Program Files\RStudio\resources\app\bin\quarto\bin\quarto.exe"
$quartoLocalData = Join-Path $siteRoot ".localappdata"

if (-not (Test-Path -LiteralPath $rscript)) { throw "Rscript was not found at $rscript" }
if (-not (Test-Path -LiteralPath $quarto)) { throw "Quarto was not found at $quarto" }

New-Item -ItemType Directory -Force -Path $quartoLocalData | Out-Null
$env:LOCALAPPDATA = $quartoLocalData
$env:LANG = "C"
$env:LC_ALL = "C"
$env:LC_CTYPE = "C"

Push-Location $siteRoot
try {
  & $rscript --vanilla scripts/build_site_fragments.R
  if ($LASTEXITCODE -ne 0) { throw "Fragment generation failed." }

  & $rscript --vanilla scripts/validate_site.R
  if ($LASTEXITCODE -ne 0) { throw "Source validation failed." }

  & $quarto render --no-clean
  if ($LASTEXITCODE -ne 0) { throw "Quarto render failed." }

  & $rscript --vanilla scripts/restore_legacy_assets.R
  if ($LASTEXITCODE -ne 0) { throw "Legacy article asset restoration failed." }

  & $rscript --vanilla scripts/validate_site.R --rendered
  if ($LASTEXITCODE -ne 0) { throw "Rendered-site validation failed." }

  Write-Host "Site build complete: $siteRoot\docs\index.html"
}
finally {
  Pop-Location
}
