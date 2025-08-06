# nlmeU

## Installation


```
devtools::install_github("agalecki/nlmeU")`
```

## Scripts

Auxiliary function

```
my_source <- function(file,  
                      pkg = "nlmeU", folder = "scriptsR4.5.1",
                      ...){
  message("#>>> --- Script ", file, " in ", pkg, " package executed.")
  fpath <- system.file(folder, file, package = pkg)
  source(fpath, echo = TRUE, ...)
}
```

Source scripts:

```
library(nlmeU)
runScript()
my_source("Ch01.R")
my_source("Ch02.R")
my_source("Ch03.R")
my_source("Ch05.R")
my_source("Ch06.R")
my_source("Ch08.R")
my_source("Ch09.R")
my_source("Ch11.R")
my_source("Ch12.R")
my_source("Ch14.R")
my_source("Ch15a.R", folder = "scriptsR4.5.1/lme4")
my_source("Ch15b.R", folder = "scriptsR4.5.1/lme4")
my_source("Ch16lme.R")  # Be patient. It takes time to execute
my_source("Ch16mer.R", folder = "scriptsR4.5.1/lme4")
my_source("Ch17part1.R")
my_source("Ch17a.R")            # See nlmeUpdK package
my_source("Ch18lme.R")
my_source("Ch18mer.R", folder = "scriptsR4.5.1/lme4")
my_source("Ch19.R")
my_source("Ch19mer.R", folder = "scriptsR4.5.1/lme4")
my_source("Ch20.2pdK1a.R")      # See nlmeUpdK package
my_source("Ch20.3influence.R")
my_source("Ch20.4simY.R")
my_source("Ch20.5Pwr.R")
my_source("Ch20a.R", folder = "scriptsR4.5.1/lme4")
```

Datasets in CSV format are stored in:

```
(csvPath <- system.file("csvData", package = "nlmeU"))
list.files(csvPath)

```

