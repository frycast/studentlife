<!-- Badges Start -->
[![Travis-CI Build Status](https://travis-ci.org/frycast/studentlife.svg?branch=master)](https://travis-ci.org/frycast/studentlife) ![Travis-CI Build Status](http://www.r-pkg.org/badges/version/studentlife) 
[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/frycast/studentlife/master?urlpath=rstudio) 
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0) 
[![Code coverage](https://codecov.io/gh/frycast/studentlife/branch/master/graph/badge.svg)](https://codecov.io/github/frycast/studentlife?branch=master)
[![Lifecycle Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/)
<!-- Badges End -->

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


<!--

DOCUMENTATION CHECKLIST


    A statement of need: Do the authors clearly state what problems the software is designed to solve and who the target audience is?
    Installation instructions: Is there a clearly-stated list of dependencies? Ideally these should be handled with an automated package management solution.
    Example usage: Do the authors include examples of how to use the software (ideally to solve real-world analysis problems).
    Functionality documentation: Is the core functionality of the software documented to a satisfactory level (e.g., API method documentation)?
    Automated tests: Are there automated tests or manual steps described so that the function of the software can be verified?
    Community guidelines: Are there clear guidelines for third parties wishing to 1) Contribute to the software 2) Report issues or problems with the software 3) Seek support


A statement of need

The authors should clearly state what problems the software is designed to solve and who the target audience is.
Installation instructions

There should be a clearly-stated list of dependencies. Ideally these should be handled with an automated package management solution.

    Good: A package management file such as a Gemfile or package.json or equivalent
    OK: A list of dependencies to install
    Bad (not acceptable): Reliance on other software not listed by the authors

Example usage

The authors should include examples of how to use the software (ideally to solve real-world analysis problems).
API documentation

Reviewers should check that the software API is documented to a suitable level.

    Good: All functions/methods are documented including example inputs and outputs
    OK: Core API functionality is documented
    Bad (not acceptable): API is undocumented
    

Community guidelines

There should be clear guidelines for third-parties wishing to:

    Contribute to the software
    Report issues or problems with the software
    Seek support

Functionality

Reviewers are expected to install the software they are reviewing and to verify the core functionality of the software.
Tests

Authors are strongly encouraged to include an automated test suite covering the core functionality of their software.

    Good: An automated test suite hooked up to an external service such as Travis-CI or similar
    OK: Documented manual steps that can be followed to objectively check the expected functionality of the software (e.g. a sample input file to assert behaviour)
    Bad (not acceptable): No way for you the reviewer to objectively assess whether the software works
    
-->

