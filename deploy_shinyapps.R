# deploy_shinyapps.R

library(rsconnect)

# 1) # Get token info from shinyapps.io > Account > Tokens.
 #rsconnect::setAccountInfo(
 #  name   = "",
 #  token  = "",
 #  secret = ""
# )

app_files <- c(
  "app.R",
  "README.md",
  "LICENSE",
  list.files("R", full.names = TRUE),
  list.files("outputs", pattern = "\\.rds$", full.names = TRUE)
)

if (dir.exists("outputs/map_cache")) {
  app_files <- c(
    app_files,
    list.files("outputs/map_cache", full.names = TRUE, recursive = TRUE)
  )
}

rsconnect::deployApp(
  appDir = ".",
  appFiles = app_files,
  appName = "procurement-risk-analytics-dashboard",
  appTitle = "Procurement Risk Analytics Dashboard",
  forceUpdate = TRUE
)