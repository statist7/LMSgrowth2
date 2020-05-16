#-------------------------------------------------------------------------------
# The ui Shiny component to load the help documentation
# (Does not perform any server functions)
#-------------------------------------------------------------------------------

.helpUI <- function(id) {
  fluidPage(
    .titleBar('help', 'Help', NULL),
    includeMarkdown(system.file('assets', 'help.md', package='LMSgrowth2'))
  )
}


.help <- function(input, output, session, globals) {
  
}