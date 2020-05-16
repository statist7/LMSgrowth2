[![Build Status](https://travis-ci.com/UCL/LMSgrowth2.svg?token=VzQyGkGCwi2xenWmKVcK&branch=master)](https://travis-ci.com/UCL/LMSgrowth2)

# LMSgrowth2

## Running the app

In R, install the package from Github using devtools. Run the following commands:

```
install.packages('devtools')
library(devtools)
install_github('UCL/LMSgrowth2', dependencies=TRUE)
```

Then run `LMSgrowth2::run_app()` to start the Shiny app.

## Deploying server

There are several ways to deploy the LMSgrowth2 Shiny app. Here are three:

1. Deploy to [shinyapps.io](https://www.shinyapps.io/). Clone this repository, open the `app.R` file in RStudio & click the Publish button. See shinyapps.io [getting started](https://shiny.rstudio.com/articles/shinyapps.html) documentation for more information.

2. Using [Docker](https://www.docker.com/). Run on the command line (specifying uploads directory on host):

```
git clone https://github.com/UCL/LMSgrowth2.git
cd LMSgrowth2
docker build --tag lmsgrowth2 .
docker run --publish 3838:3838 -v /uploads/dir/on/host:/srv/shiny-server/LMSgrowth2/inst/uploads lmsgrowth2
```

Then open your browser [http://localhost:3838/](http://localhost:3838/)

3. Using [Vagrant](https://www.vagrantup.com/). Run on the command line.

```
git clone https://github.com/UCL/LMSgrowth2.git
cd LMSgrowth2/vagrant
vagrant up
```

Then open your browser [http://localhost:3001/](http://localhost:3001/)

## Development setup

### Prerequisites

Following packages are used during development and must be installed in the R user library

```
install.packages(c('devtools', 'usethis', 'testthat'))
```

### Setup

- Clone the [LMSgrowth2 repository](https://github.com/UCL/LMSgrowth2)

- Open the project in RStudio

- Install the project package dependencies:
  - `packrat::restore()`

- Test:
  - Build > Test Package

### Run

- To run the app `devtools::load_all(); LMSgrowth2::run_app()`

### Updating Packrat dependencies

1. List all packages where updates are available using `old.packages()`. 
2. Either, update a specific package using, for example, `install.packages('plotly')` or all packages using `update.packages()`.
3. Run LMSgrowth2 in the usual way and check everything is working. 
4. Once confirmed, save the updates to Packrat using `packrat::snapshot(snapshot.sources=F)`.
