## Resubmission
This is a resubmission, the following have been fixed as requested by Martina Schmirl:

  * Please only capitalize sentence beginnings and names in the description 
    text. (f.i.: ... ecological momentary assessment results ...)
  
  * We see you have in your Readme vignette:
    ``` r
    # install.packages("devtools")
    devtools::install_github("frycast/studentlife")
    ```
    So when checking the vignette, we always install an extra copy of your 
    package which does not make sense nor is this permitted. So please make 
    this a comment that is not evaluated when your vignette code gets executed.
  
  * The LICENSE file is only needed if you have additional restrictions to 
    the GPL-3 which you have not? In that case omit the file and its 
    reference in the DESCRIPTION file.
  
  * All examples are wrapped in \donttest{}.
    It would be great if you could use another dataset for most examples to 
    enable automatic testing.
  
  * Please do not write to the user filespace. If you
    really have to write out something, use tempdir() if the user is not
    asked and in examples.
    (d <- tempdir() )






## Test environments

* Local Windows 10 install (R 3.5.3)
* Ubuntu 14.04 Trusty (on Travis-CI) (R devel, 3.6.0 and 3.5.3)
* Mac OS X 10.13.3 High Sierra (on Travis-CI) (R 3.6.0)
* winbuilder (R devel and 3.6.0)

## R CMD Check results
There were no ERRORS or WARNINGS.

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Daniel Fryer <d.fryer@latrobe.edu.au>'

  New submission

  Possibly mis-spelled words in DESCRIPTION:
  pre (15:45)
  
  This is the correct spelling.
  
## Downstream dependencies
There are no downstream dependencies for this package
