# nlmeU

## Installation

```
devtools::install_github("agalecki/nlmeU")
```

## Scripts


Source scripts:

```
library(nlmeU)
runScript()
runScript("Ch01.R")
runScript("Ch02.R")
runScript("Ch03.R")
runScript("Ch05.R")
runScript("Ch06.R")
runScript("Ch08.R")
runScript("Ch09.R")
runScript("Ch11.R")
runScript("Ch12.R")
runScript("Ch14.R")
runScript("Ch15a.R")
runScript("Ch15b.R")
runScript("Ch16lme.R")  # Be patient. It takes time to execute
runScript("Ch16mer.R")
runScript("Ch17part1.R")
runScript("Ch17a.R")            # See nlmeUpdK package
runScript("Ch18lme.R")
runScript("Ch18mer.R")
runScript("Ch19.R")
runScript("Ch19mer.R")
runScript("Ch20.2pdK1a.R")      # See nlmeUpdK package
runScript("Ch20.3influence.R")
runScript("Ch20.4simY.R")
runScript("Ch20.5Pwr.R")
runScript("Ch20a.R")
```

Datasets in CSV format are stored in:

```
(csvPath <- system.file("csvData", package = "nlmeU"))
list.files(csvPath)

```

