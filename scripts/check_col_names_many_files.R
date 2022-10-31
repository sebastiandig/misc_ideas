library("tidyverse")
library("readxl")
library("magrittr")

# TODO: need to create files to read, so not reliant on hard path
# select path
dir.path <- "C:/Users/spd19/OneDrive/Documents/College/IMaRS/Cruise Data/"

# get paths for all files wanting
files_change <- fs::dir_ls(
  dir.path,
  recurse = TRUE,
  regexp = "[^~$]CDOM.*_run_.*"
) %>%
  tibble(files = .) %>%
  filter(!str_detect(files, "old")) 

# load all files into a list column
test <-
  files_change %>%
  mutate(
  data = map(files,
      ~ read_xlsx(., skip = 6)))
  
# enter lists and create column for names
test2 <- test %>%
  mutate(
    names = map(test$data, ~   (names(.x))))
  
# or print names
map(test$data, ~   names(.x) # %>% tibble(files = .)
    )
