options(repos = c(CRAN = "https://packagemanager.posit.co/cran/latest"))

packages <- c(
  "DBI",
  "RPostgres",
  "digest",
  "jsonlite"
)

install.packages(packages)

