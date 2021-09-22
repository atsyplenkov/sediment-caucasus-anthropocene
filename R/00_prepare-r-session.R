# install pacman to streamline further package installation
# solution by Ilya Kashnitsky from: https://github.com/ikashnitsky/the-lancet-2018/blob/master/R/0-prepare-r-session.R
if (!require("pacman", character.only = TRUE)){
  install.packages("pacman", dep = TRUE)
  if (!require("pacman", character.only = TRUE))
    stop("Package not found")
}

# these are the required packages
pkgs <- c(
  "tidyverse", 
  "magrittr",
  "readxl",
  "writexl",
  "sf",
  "MASS",
  "hrbrthemes",
  "mapview",
  "extrafont",
  "fingerPro",
  "ggsci",
  "here",
  "scales",
  "lubridate",
  "pangaear",
  "repmis",
  "zoo",
  "fasstr",
  "ggpubr"
)

# install the missing packages
# only run if at least one package is missing
if(!sum(!p_isinstalled(pkgs))==0){
  p_install(
    package = pkgs[!p_isinstalled(pkgs)], 
    character.only = TRUE
  )
}

# install development versions
# atslib 
devtools::install_github("atsyplenkov/atslib")

# ChangePointAnalyzer
# edited version from CRAN
install.packages("bench")

install.packages("additional/ChangePointTaylor_0.1.0.tar",
                 repos = NULL,
                 type = "source")


# Install Noto Sans font
temp <- tempfile()
unzip("additional/Noto_Sans.zip",
      exdir = temp)
extrafont::font_import(paths = temp)
unlink(temp)
