# LMSgrowth2

## Prerequisites

Following packages are used during development and must be installed in the R user library

```
install.packages(c('devtools', 'usethis', 'testthat'))
```

## Project setup using RStudio

- Clone the LMSgrowth2 repository
    - URL: git@github.com:UCL/LMSgrowth2.git

- Open the project in RStudio

- Install the project package dependencies:
  - `packrat::restore()`
  
- Test:
  - Build > Test Package

- Load the LMSgrowth2 package for development
    - Build > Load All (Ctrl+Shift+L)
    - (or from tab) Build > Load All
    - Type `LMSgrowth2::runPackage()` in the R console to run

