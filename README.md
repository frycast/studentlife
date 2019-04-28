[![Travis-CI Build Status](https://travis-ci.org/frycast/studentlife.svg?branch=master)](https://travis-ci.org/frycast/studentlife) ![Travis-CI Build Status](http://www.r-pkg.org/badges/version/studentlife) [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/frycast/studentlife/master)

### Install from GitHub with
```r
# install.packages("devtools")
devtools::install_github("frycast/studentlife")
```

Details on the dataset are available [here](https://studentlife.cs.dartmouth.edu). Once installed, you can download and extract the data within R:

```
d <- "studentlife"
library(studentlife)
download_studentlife(dest = d)
```

Then you can use the interactive menu to browse the tables and schemas:

```r
studs <- studentlife::load_SL_tibble(location = d)
```

For more information see the help files:

```r
?download_studentlife
?load_SL_tibble
```




