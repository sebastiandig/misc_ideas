# to examine all tasks open on your machine ----------------
system("tasklist /?")
# to close a program ---------------------------------------
system("taskkill /?")

# /FI - filter
# WINDOWTITLE - the call to filter for window
# eq = equal
# <name of file> - Excel = actual name of the window when its opened
# /F = forcibly
# /V = verbose
# *NOTE*: this will kill all excel pages because
    # of ShellExperienceHost.exe (jump list for excel)
    # this .exe I guess allows jumping between excel files, but only
    # the most active one will be able to be searched

# ---- to find ----
# system('tasklist /FI "WINDOWTITLE eq <file-name> - Excel" /V')

# ---- active (i.e. last to view) ----
# Image Name                     PID Session Name        Session#    Mem Usage
# ========================= ======== ================ =========== ============
#   EXCEL.EXE                    29816 Console                    1    384,528 K

# ---- non-active ----
# INFO: No tasks are running which match the specified criteria.

# ---- to kill ----
# system('taskkill /FI "WINDOWTITLE eq <file-name> - Excel" /F')

library(magrittr)

# create two txt files
ex_data <- tibble::tribble(~x, 1, 2, 2, 3)
write.csv(ex_data, "./data/raw/test1.txt")
write.csv(ex_data, "./data/raw/test2.txt")

# open both outside of rstudio
for (i in 1) {
  shell.exec(here::here("data", "raw", "test1.txt"))
  shell.exec(here::here("data", "raw", "test2.txt"))
}

# finds both open .txt files in tasklist
system('tasklist /FI "IMAGENAME eq notepad.exe" /V')

# finds the file called test1 (2-ways)
system('tasklist /FI "WINDOWTITLE eq test1 - Notepad" /V')
system('tasklist /FI "WINDOWTITLE eq test1 - *" /V') # * is wildcard

# will kill only test1 file, and leave test2 open
system('taskkill /FI "WINDOWTITLE eq test1 - Notepad*" /F')



