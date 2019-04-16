# LMSgrowth2

## Development setup using RStudio

- Clone the LMSgrowth2 repository
  - File > New Project > Version Control > Git > 
    - Repository URL: git@github.com:UCL/LMSgrowth2.git
    - Click 'Create Project'

- Once the project is open in RStudio, in R:
  - install.packages('packrat')
  - packrat::init()
  - packrat::restore()

- Set the RStudio project options for build tools (you can leave the defaults)
  - Build > Configure Build Tools...
  - Click 'OK'

- Close the project

- Reopen the LMSgrowth2 project

- Build and install the LMSgrowth2 package:
    - Click on Build > Install & Restart
    - Type `launchApp()` in the R console to test

