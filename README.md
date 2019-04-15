# LMSgrowth2

## Development setup using RStudio

- Clone LMSgrowth2 (you can do within RStudio):
  - File > New Project > Version Control > Git > 
    - Repository URL: git@github.com:UCL/LMSgrowth2.git
    - Click 'Create Project'

- Then, in R:
  - (if necessary:) install.packages('packrat')
  - packrat::init()
  - packrat::restore()

- Use RStudio project options for build tools
  - Build > Configure Build Tools...

- Close project & reopen project to get build options

- Click on Build > Install & Restart
  - Will automatically install and load LMSgrowth2 in R session
  - Type `launchApp()` to test
