
# Define the list of packages
pkgs <- c("plyr", "reshape", "RLRsim", "WWGbook", "ellipse", "lme4", "nlmeU")

# Function to check and install/update packages from CRAN
install_or_update <- function(pkg) {
  if (pkg %in% installed.packages()[, "Package"]) {
    installed_version <- packageVersion(pkg)
    cran_version <- tryCatch({
      available <- available.packages()
      as.character(available[pkg, "Version"])
    }, error = function(e) NA)
    if (!is.na(cran_version) && installed_version < cran_version) {
      cat(sprintf("Updating %s from version %s to %s\n", pkg, installed_version, cran_version))
      install.packages(pkg)
    } else {
      cat(sprintf("%s is up-to-date (version %s)\n", pkg, installed_version))
    }
  } else {
    cat(sprintf("Installing %s from CRAN\n", pkg))
    install.packages(pkg)
  }
}

# Install or update each package
for (pkg in pkgs) {
  install_or_update(pkg)
}

# Verify installed versions
cat("\nInstalled package versions:\n")
for (pkg in pkgs) {
  if (pkg %in% installed.packages()[, "Package"]) {
    cat(sprintf("%s: %s\n", pkg, packageVersion(pkg)))
  } else {
    cat(sprintf("%s: Not installed\n", pkg))
  }
}

# Test nlmeU documentation
if ("nlmeU" %in% installed.packages()[, "Package"]) {
  library(nlmeU)
  cat("\nTesting nlmeU documentation:\n")
  ?nlmeU
}

  


