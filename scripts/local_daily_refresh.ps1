[CmdletBinding()]
param(
  [ValidateSet("Full", "Lineups", "Site")]
  [string]$Mode = "Full",

  [ValidatePattern("^\d{4}-\d{2}-\d{2}$")]
  [string]$PbpEnd = (Get-Date).AddDays(-1).ToString("yyyy-MM-dd"),

  [ValidatePattern("^\d{4}-\d{2}-\d{2}$")]
  [string]$SlateDate = (Get-Date).ToString("yyyy-MM-dd"),

  [switch]$Fast,
  [switch]$OpenSite
)

$ErrorActionPreference = "Stop"

$siteRoot = Split-Path -Parent $PSScriptRoot
$workspaceRoot = Split-Path -Parent $siteRoot
$rscript = "C:\Program Files\R\R-4.4.1\bin\Rscript.exe"
$rExe = "C:\Program Files\R\R-4.4.1\bin\R.exe"
$rLibrary = Join-Path $workspaceRoot "sabrhoodR\.Rlib"
$packagePath = Join-Path $siteRoot "packages\sabrhoodR"
$siteBuilder = Join-Path $PSScriptRoot "build_site.ps1"
$siteIndex = Join-Path $siteRoot "docs\index.html"

foreach ($requiredPath in @($rscript, $rExe, $rLibrary, $packagePath, $siteBuilder)) {
  if (-not (Test-Path -LiteralPath $requiredPath)) {
    throw "Required local dependency was not found: $requiredPath"
  }
}

foreach ($value in @($PbpEnd, $SlateDate)) {
  $parsedDate = [DateTime]::MinValue
  if (-not [DateTime]::TryParseExact(
      $value,
      "yyyy-MM-dd",
      [Globalization.CultureInfo]::InvariantCulture,
      [Globalization.DateTimeStyles]::None,
      [ref]$parsedDate
    )) {
    throw "Dates must be real calendar dates written as YYYY-MM-DD. Invalid value: $value"
  }
}

$env:R_LIBS_USER = $rLibrary
$env:SABRHOOD_PBP_END = $PbpEnd
$env:SABRHOOD_HISTORY_END = $PbpEnd
$env:SABRHOOD_DATE = $SlateDate
$env:SABRHOOD_N_SIMS = if ($Fast) { "5000" } else { "20000" }
$env:SABRHOOD_PLAYER_N_SIMS = if ($Fast) { "500" } else { "2000" }

# The local workflow must write the same public products used by Quarto.
foreach ($environmentName in @(
    "SABRHOOD_DERIVED_DIR",
    "SABRHOOD_GRAPHICS_DIR",
    "SABRHOOD_DAILY_OUTPUT_DIR"
  )) {
  Remove-Item "Env:$environmentName" -ErrorAction SilentlyContinue
}

$requiredPackages = @(
  "baseballr",
  "dplyr",
  "ggplot2",
  "jsonlite",
  "knitr",
  "Lahman",
  "rlang",
  "rmarkdown",
  "tibble",
  "withr"
)
$quotedPackages = ($requiredPackages | ForEach-Object { "'$_'" }) -join ","
$packageCheck = @"
packages <- c($quotedPackages)
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
cat(missing, sep = '\n')
"@
$missingPackages = @(& $rscript --vanilla -e $packageCheck)
if ($LASTEXITCODE -ne 0) {
  throw "R package verification failed."
}
if ($missingPackages.Count -gt 0) {
  throw @"
This computer is missing required R packages: $($missingPackages -join ", ")
Run the one-time setup first:
  .\scripts\setup_local_refresh.ps1
"@
}

$script:stepNumber = 0
$script:stepTotal = if ($Mode -eq "Full") { 19 } elseif ($Mode -eq "Lineups") { 10 } else { 1 }

function Write-Stage {
  param([string]$Label)
  $script:stepNumber += 1
  Write-Host ""
  Write-Host "[$($script:stepNumber)/$($script:stepTotal)] $Label" -ForegroundColor Cyan
}

function Invoke-RScript {
  param(
    [Parameter(Mandatory = $true)][string]$Label,
    [Parameter(Mandatory = $true)][string]$RelativePath,
    [switch]$Optional
  )

  Write-Stage $Label
  $started = Get-Date
  & $rscript --vanilla (Join-Path $siteRoot $RelativePath)
  $exitCode = $LASTEXITCODE
  $elapsed = [Math]::Round(((Get-Date) - $started).TotalSeconds)

  if ($exitCode -ne 0) {
    if ($Optional) {
      Write-Warning "$Label did not complete. The workflow will try to use the last good cached version."
      return $false
    }
    throw "$Label failed after $elapsed seconds. Fix this stage, then rerun the same command."
  }

  Write-Host "Completed in $elapsed seconds." -ForegroundColor DarkGreen
  return $true
}

function Install-SabrhoodPackage {
  Write-Stage "Install the current local sabrhoodR package"
  & $rExe CMD INSTALL "--library=$rLibrary" $packagePath
  if ($LASTEXITCODE -ne 0) {
    throw "The local sabrhoodR package could not be installed."
  }
}

function Get-LineupRowCount {
  $lineupPath = Join-Path $siteRoot "data\derived\daily-batting-orders.csv"
  if (-not (Test-Path -LiteralPath $lineupPath)) { return 0 }
  return @(Import-Csv -LiteralPath $lineupPath).Count
}

function Test-HasSlate {
  return -not (Test-Path -LiteralPath (Join-Path $siteRoot ".private-data\no-slate"))
}

function Invoke-SiteBuild {
  Write-Stage "Generate fragments, validate, and render the complete Quarto site"
  & $siteBuilder
  if ($LASTEXITCODE -ne 0) {
    throw "The Quarto site build failed."
  }
}

function Invoke-LineupRefresh {
  Invoke-RScript "Check today's probable starters and posted batting orders" "scripts\refresh_daily_lineups.R" | Out-Null

  if (-not (Test-HasSlate)) {
    Write-Host "There is no MLB slate for $SlateDate. Player and team simulations are skipped." -ForegroundColor Yellow
    $script:stepNumber += 4
    return
  }

  Invoke-RScript "Rerun today's team simulations" "scripts\build_daily_team_simulations.R" | Out-Null

  $lineupRows = Get-LineupRowCount
  if ($lineupRows -ge 9) {
    Invoke-RScript "Recalculate player event probabilities" "scripts\build_daily_player_probabilities.R" | Out-Null
    Invoke-RScript "Rerun player simulations with posted lineups" "scripts\build_daily_player_simulations.R" | Out-Null
  } else {
    Write-Host "Only $lineupRows lineup rows are posted. Player simulations keep their last valid state." -ForegroundColor Yellow
    $script:stepNumber += 2
  }

  Invoke-RScript "Archive eligible pregame predictions" "scripts\archive_projection_snapshot.R" | Out-Null
}

Push-Location $siteRoot
try {
  Write-Host ""
  Write-Host "The SABRhood local refresh" -ForegroundColor Red
  Write-Host "Mode: $Mode | completed PBP through: $PbpEnd | today's slate: $SlateDate"
  if ($Fast) {
    Write-Host "Fast simulation counts are enabled for previewing." -ForegroundColor Yellow
  }

  if ($Mode -eq "Site") {
    Invoke-SiteBuild
  } else {
    Install-SabrhoodPackage

    if ($Mode -eq "Lineups") {
      Invoke-LineupRefresh
      Invoke-RScript "Refresh branded graphics after the lineup check" "scripts\build_graphics_feed.R" | Out-Null
      Invoke-RScript "Check public-product freshness" "scripts\build_refresh_health.R" | Out-Null
      Invoke-RScript "Update public-data checksums and dates" "scripts\update_derived_manifest.R" | Out-Null
      Invoke-SiteBuild
    } else {
      Invoke-RScript "Download completed MLB games into the private PBP store" "scripts\update_pbp.R" | Out-Null
      Invoke-RScript "Grade prior projection snapshots against actual results" "scripts\settle_projection_ledger.R" | Out-Null
      Invoke-RScript "Rebuild every PBP-derived table and bullpen product" "scripts\build_pbp_products.R" | Out-Null

      Invoke-RScript "Refresh season-level FanGraphs source tables" "scripts\refresh_fangraphs_season.R" -Optional | Out-Null
      Invoke-RScript "Rebuild season leaderboards, awards, and team WAR products" "scripts\build_fangraphs_products.R" -Optional | Out-Null
      Invoke-RScript "Update rolling award-race history checkpoints" "scripts\refresh_award_history.R" -Optional | Out-Null

      Invoke-RScript "Rebuild history, milestones, records, and anniversary notes" "scripts\build_history_products.R" | Out-Null
      Invoke-RScript "Rebuild story scores, spotlights, matchups, and newsletter products" "scripts\build_editorial_products.R" | Out-Null
      Invoke-RScript "Refresh the Triple-A watch" "scripts\refresh_aaa.R" -Optional | Out-Null
      Invoke-RScript "Pull today's games, rosters, probables, lineups, and weather" "scripts\pull_daily_game_info.R" | Out-Null

      if (Test-HasSlate) {
        Invoke-RScript "Run today's team win and scoring simulations" "scripts\build_daily_team_simulations.R" | Out-Null
        $lineupRows = Get-LineupRowCount
        if ($lineupRows -ge 9) {
          Invoke-RScript "Calculate today's player event probabilities" "scripts\build_daily_player_probabilities.R" | Out-Null
          Invoke-RScript "Run today's player simulations" "scripts\build_daily_player_simulations.R" | Out-Null
        } else {
          Write-Host "Only $lineupRows lineup rows are posted. Run -Mode Lineups closer to first pitch." -ForegroundColor Yellow
          $script:stepNumber += 2
        }
        Invoke-RScript "Archive eligible pregame predictions" "scripts\archive_projection_snapshot.R" | Out-Null
      } else {
        Write-Host "There is no MLB slate for $SlateDate. Daily simulations are skipped." -ForegroundColor Yellow
        $script:stepNumber += 4
      }

      Invoke-RScript "Rebuild the complete branded graphics feed" "scripts\build_graphics_feed.R" | Out-Null
      Invoke-RScript "Check that every public product is current enough to publish" "scripts\build_refresh_health.R" | Out-Null
      Invoke-RScript "Update public-data checksums and source dates" "scripts\update_derived_manifest.R" | Out-Null
      Invoke-SiteBuild
    }
  }

  Write-Host ""
  Write-Host "SUCCESS: the complete local site is ready." -ForegroundColor Green
  Write-Host "Publish this folder: $siteRoot\docs"
  Write-Host "Local homepage: $siteIndex"
  Write-Host "Review changed files with: git status --short"

  if ($OpenSite) {
    Start-Process $siteIndex
  }
}
finally {
  Pop-Location
}
