# ============================================================================ #
# 
# ---- Tips About Windows System ----
# 
# ============================================================================ #  


# ============================================================================ #
# ---- Find Open Tasks/Programs on Machine  ----
# ============================================================================ #
# This will help find open windows/programs.

# ---- List all options for `tasklist`
system('tasklist /?')
# - /FI - filter
# - eq = equal (used with /FI)
#   - can use "*" as a wildcard
# - /V = verbose

# ---- Show all running tasks (i.e. all open programs)
system('tasklist')

# ---- Filter task by `Image Name` or `Window Title`
# Note: not case sensitive

# Image Name = the executable name, find from `tasklist`
# - flag is `IMAGENAME`
# - needs to be full name or use a wildcard, not partial 
#   - e.g. `excel` won't work but `excel*` will
# - ex system('tasklist /FI "IMAGENAME eq <image name>" /V')
# system('tasklist /FI "IMAGENAME eq EXCEL.EXE" /V')
# system('tasklist /FI "IMAGENAME eq excel.exe" /V')

# Window Title = the name of the window
# - flag is `WINDOTITLE`
# - NOTE: 
#   - takes time to run
#   - may not be the exact name at the top of the window
# system('tasklist /FI "WINDOWTITLE eq <file-name> - Excel" /V')

# ---- Output from filter

# Active (i.e. most recently active window, not minimized or behind another)

# Image Name                     PID Session Name        Session#    Mem Usage
# ========================= ======== ================ =========== ============
#   EXCEL.EXE                    29816 Console                    1    384,528 K

# ---
# Non-active (i.e. could not find matching the filter)

# INFO: No tasks are running which match the specified criteria.


# ============================================================================ #


# ============================================================================ #
# ---- Close Tasks/Programs on Machine  ----
# ============================================================================ #
# This will show how to close/kill an open file/program

# ---- List all options for `taskkill`
system("taskkill /?")
# - /FI - filter
# - eq = equal (used with /FI)
# - /F = forcibly
# - /V = verbose
# - WINDOWTITLE - filter for window name
# - <name of file> - Excel = actual name of the window when its opened
#   - *NOTE*: 
#     - this will kill all excel pages because of `ShellExperienceHost.exe` 
#       (jump list for excel). This `.exe` I guess allows jumping between excel 
#       files, but only the most recently active one will be able to be searched

# ---- Kill task by `Image Name` or `Window Title`
# system('taskkill /FI "IMAGENAME eq <image name>" /F')
# system('taskkill /FI "WINDOWTITLE eq <file-name> - Excel" /F')

# Full example
if (FALSE) {

  # create two txt files
  ex_data <- data.frame(x = c(1, 2, 2, 3, 4))
  
  if (!file.exists("./data/raw/test1.txt")) 
    write.csv(ex_data, "./data/raw/test1.txt")
  if (!file.exists("./data/raw/test2.txt")) 
    write.csv(ex_data, "./data/raw/test2.txt")

  # open both outside of rstudio
  {
    shell.exec(normalizePath("./data/raw/test1.txt"))
    shell.exec(normalizePath("./data/raw/test2.txt"))
  }

  # find both open notepad `.txt` files in `tasklist`
  system('tasklist /FI "IMAGENAME eq notepad.exe" /V')

  # find test1 file only (2-ways)
  system('tasklist /FI "WINDOWTITLE eq test1 - Notepad" /V')
  # system('tasklist /FI "WINDOWTITLE eq test1 - *" /V') # wild card -> `*`
  
  readline("Hit [Enter} when ready to kill! ")
  
  # kill only test1 file and leave test2 open
  system('taskkill /FI "WINDOWTITLE eq test1 - Notepad" /F')
  # system('taskkill /FI "WINDOWTITLE eq test1*" /F')
  
  # kill both test1 and test2 file
  # system('taskkill /FI "WINDOWTITLE eq test*" /F')

}

# ============================================================================ #
# ---- Open Chrome/MS Edge with Specific User Profile and URL ----
# ============================================================================ #
# Dec 07, 2023
# This will open a new tab at the specified user profile to the specific URL

# ---- Use `shell()` to Run CMD Commands
# Basic structure:
# shell("start chrome /profile-directory=\"<profile name here>\" https://www.<unquoted website name>") 
# shell("start msedge /profile-directory=\"<profile name here>\" https://www.<unquoted website name>") 
# - start - tells CMD that will run a program
# - specify application
#   - chrome - `chrome`
#   - msedge - `edge`
# - /profile-directory= to select profile (default: `default`)
# - URL with `https://www.`

# if need help determining the profile name
# 1. open chrome/edge and select the profile 
if (FALSE) {
  shell("start chrome")  
  shell("start  edge")
}

# 2. enter URL: 
#    - chrome: `chrome://version/`
#    - edge:   `edge://version/`
# 3. find the "Profile Path"
# 4. copy the name after "User Data"; this is the profile name

# ---- Open chrome/msedge to profile and URL
# after finding the profile name, change name after `/profile-directory=`
# - NOTE: this needs to include `\"` to escape the `"`
# shell("start chrome /profile-directory=\"<profile name here>\" https://www.<unquoted website name>") 
# shell("start msedge /profile-directory=\"<profile name here>\" https://www.<unquoted website name>") 

# example for two different profiles
# chrome
if (FALSE) {
  shell("start chrome /profile-directory=\"Default\" https://www.google.com") 
  shell("start chrome /profile-directory=\"Profile 1\" https://www.google.com") 
}

# edge
if (FALSE) {
  shell("start msedge /profile-directory=\"Default\" https://www.bing.com")
  shell("start msedge /profile-directory=\"Profile 1\" https://www.bing.com")
}
# ============================================================================ #
