[CmdletBinding()]
param()

$ErrorActionPreference = "Stop"

$siteRoot = Split-Path -Parent $PSScriptRoot
$workspaceRoot = Split-Path -Parent $siteRoot
$rscript = "C:\Program Files\R\R-4.4.1\bin\Rscript.exe"
$rExe = "C:\Program Files\R\R-4.4.1\bin\R.exe"
$rLibrary = Join-Path $workspaceRoot "sabrhoodR\.Rlib"
$packagePath = Join-Path $siteRoot "packages\sabrhoodR"

if (-not (Test-Path -LiteralPath $rscript)) { throw "Rscript was not found at $rscript" }
if (-not (Test-Path -LiteralPath $rExe)) { throw "R was not found at $rExe" }
if (-not (Test-Path -LiteralPath $packagePath)) { throw "sabrhoodR was not found at $packagePath" }

New-Item -ItemType Directory -Force -Path $rLibrary | Out-Null
$env:R_LIBS_USER = $rLibrary
$rLibraryForR = $rLibrary.Replace("\", "/").Replace("'", "\'")

Write-Host ""
Write-Host "One-time SABRhood local refresh setup" -ForegroundColor Red
Write-Host "R packages will be installed in: $rLibrary"

$cranPackages = @(
  "remotes",
  "jsonlite",
  "dplyr",
  "rlang",
  "tibble",
  "withr",
  "ggplot2",
  "knitr",
  "rmarkdown",
  "Lahman"
)
$quotedPackages = ($cranPackages | ForEach-Object { "'$_'" }) -join ","
$installExpression = @"
lib <- '$rLibraryForR'
packages <- c($quotedPackages)
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(
  missing,
  lib = lib,
  repos = 'https://cloud.r-project.org',
  dependencies = c('Depends', 'Imports')
)
"@

Write-Host ""
Write-Host "[1/3] Install required CRAN packages" -ForegroundColor Cyan
& $rscript --vanilla -e $installExpression
if ($LASTEXITCODE -ne 0) { throw "CRAN package installation failed." }

Write-Host ""
Write-Host "[2/3] Install the pipeline's pinned baseballr version" -ForegroundColor Cyan
$baseballrExpression = @"
lib <- '$rLibraryForR'
if (!requireNamespace('baseballr', quietly = TRUE)) {
  remotes::install_url(
    'https://github.com/BillPetti/baseballr/archive/refs/tags/v1.6.0.tar.gz',
    lib = lib,
    dependencies = NA,
    upgrade = 'never'
  )
}
"@
& $rscript --vanilla -e $baseballrExpression
if ($LASTEXITCODE -ne 0) { throw "baseballr installation failed." }

Write-Host ""
Write-Host "[3/3] Install the current local sabrhoodR package" -ForegroundColor Cyan
& $rExe CMD INSTALL "--library=$rLibrary" $packagePath
if ($LASTEXITCODE -ne 0) { throw "The local sabrhoodR package installation failed." }

$allPackages = $cranPackages + @("baseballr", "sabrhoodR")
$quotedChecks = ($allPackages | ForEach-Object { "'$_'" }) -join ","
$checkExpression = @"
packages <- c($quotedChecks)
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop('Still missing: ', paste(missing, collapse = ', '))
cat('All local refresh dependencies are ready.\n')
"@
& $rscript --vanilla -e $checkExpression
if ($LASTEXITCODE -ne 0) { throw "Dependency verification failed." }

Write-Host ""
Write-Host "SETUP COMPLETE" -ForegroundColor Green
Write-Host "Next command:"
Write-Host "  .\scripts\local_daily_refresh.ps1 -OpenSite"
