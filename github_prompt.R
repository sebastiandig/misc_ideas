# ============================================================================ #
#
# ---- At GitHub Info to Prompt ----
#
# ============================================================================ #

# Add GitHub branch name, if you need to commit a file and if you need to
# push commits.
# 
# This would he put in a your computuer `.Rprofile`. You can access this file by
# usethis::edit_r_profile(scope = "user")
# 
# You can also save this to another `.Rprofile` file and source it in the 
# project level `.Rprofile` like `source("C:/<user>/<dir 1>/<dir 2>/.Rprofile")`

.First <- function() {
  options(continue = " ")
  
  chg_prompt <- function(...) {
    
    # get project path
    proj_path <- here::here()
    
    # get git branch
    git_branch <-
      suppressWarnings(
        system(
          "git rev-parse --abbrev-ref HEAD",
          ignore.stderr = TRUE,
          intern = TRUE
        )
      )
    
    git_msg <- ""
    
    # if branch exists
    if (length(git_branch) != 0) {
      # initialize message
      git_msg <- paste0(" ", proj_path, " @", git_branch)
      
      # extract status
      git_status <-
        suppressWarnings(
          system(
            "git status -sb",
            ignore.stderr = TRUE,
            intern = TRUE
          )
        )
      
      # check if ahead
      if (any(grepl("ahead", git_status))) {
        git_msg <- paste(git_msg, "⬆︎")
      }
      
      # check if modified files
      if (any(grepl("^ M", git_status[-1]))) {
        git_msg <- paste(git_msg, "︎M")
      }
      
      # check if untracked
      if (any(grepl("^??", git_status[-1]))) {
        git_msg <- paste(git_msg, "✘")
      }
      
      # check other
      if (any(!grepl("ahead|^ M|^??", git_status[-1]))) {
        git_msg <- paste(git_msg, "O")
      }
    }
    
    console_msg <-
      paste0(
        "[",
        format(Sys.time(), "%H:%M"),
        git_msg,
        "] > "
      )
    
    
    options(prompt = console_msg)
    
    invisible(TRUE)
    
    # ---- end of function chg_prompt
  }
  
  chg_prompt()
  
  addTaskCallback(chg_prompt)
  
}
