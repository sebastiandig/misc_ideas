# set up Git with computer
# <https://usethis.r-lib.org/articles/git-credentials.html>

# step 1. Create GitHub Token for this computer
usethis::create_github_token()

# Step 2. Select  `Tokens (Classic)` instead of `Fine Grain Token` 
  # Note: The token is used as either SSH or HTTPS key
  # Edit: Token Name
  # Set expiration: >=90 days
  # Scope: 
    # check:
      # repo,
      # workflow
      # user
      # write:discussion
      # admin:gpg_key
  # Generate Token

# Step 4. Check if have git
  # Tools > Global Options > Git/SVN > Git Executables 
  # if no path exists, then <https://git-scm.com/downloads>
  # download for windows, use recommneded settings
  
  # restart Rstudio, check again 

# Step 5. Copy token
gitcreds::gitcreds_set()

# Step 6. Set Username and email
# uncomment and fill edit user.name and user.email
  # usethis::use_git_config(
  #   user.name = "<Set Username>",
  #   user.email = "<Set email>")



# Step 7. Check Personal Access Token (PAT)
usethis::git_sitrep()
 

# Step 8. Pull GitHub Repo
  # Go to GitHub project > code tab > local > copy HTTPs url

# Step 9. Start Existing Project
  # File > New Project > Version Control > Git
  # Paste URL and name folder on your computer
  # Check `open in new session`
  # "Create Project"
