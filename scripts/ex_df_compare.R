library("tidyverse")
library("cli")
# converting results of janitor::compare_df_cols to tibble with columns for 
# each mismatched column in original and rows for the type
# can use unique to see what the differences are

ex_file <- here::here("data", "raw", "test_compare_df.xlsx")

# create reproducible example
if (!fs::file_exists(ex_file)) {
  sheet1 <- 
    tibble::tribble(~x1,  ~x2,	   ~x3,	                ~x4,
             "1/1/2022",  542, "happy",	          "give me", 
             "1/1/2022",  457,  "test",	           "to me ",
             "1/1/2022",  342, "today",	            "plase", 
             "1/1/2022",  467,  "love",	              "idk",
             "1/1/2022", 3445,  "lost",	"this make no sense") %>%
 mutate(
   x1 = lubridate::mdy(x1)
 )

sheet2 <- 
    tibble::tribble(~x1,           ~x2,	   ~x3,	                ~x4,
                    "1/1/2022",  "542", "happy",	          "give me", 
                    "1/1/2022",  "457",  "test",	           "to me ",
                    "1/1/2022",  "342", "today",	            "plase", 
                    "1/1/2022",  "467",  "love",	              "idk",
                    "1/1/2022", "3445",  "lost",	"this make no sense")

sheet3 <- 
  tibble::tribble(~x1,          ~x2,	   ~x3,	                ~x4,
                  "1/1/2022",  TRUE, "happy",	          "give me", 
                  "1/1/2022", FALSE,  "test",	           "to me ",
                  "1/1/2022", FALSE, "today",	            "plase", 
                  "1/1/2022",  TRUE,  "love",	              "idk",
                  "1/1/2022",  TRUE,  "lost",	"this make no sense") 

map2(.x = list(sheet1,sheet2, sheet3), .y = c("sheet1","sheet2", "sheet3"), 
     ~xlsx::write.xlsx( .x, file = ex_file, append = TRUE, sheet = .y))

}

# load fake data
test <- 
  readxl::excel_sheets(ex_file) %>%
  tibble(sheet = .) %>%
  mutate(
    data = map(sheet, ~ readxl::read_xlsx(ex_file, sheet = .x))
    )

issues_sheet <- 
  janitor::compare_df_cols(test$data, return = "mismatch") %>%
  tibble() %>%
  pivot_longer(
    cols = 2:ncol(.),
    names_to = "sheeet",
    values_to = "type"
  ) %>%
  pivot_wider(
    id_cols = sheeet,
    names_from = column_name,
    values_from = type,
    names_sep = "_"         
  ) %>%
  mutate(
    sheet_og = test$sheet,
    .before = sheeet
  ) 


for (i  in 3:ncol(issues_sheet)) {
  cat("----------\n\n----------\n\n")
  cli::cli_inform( "Checking column: {.code {col_yellow(names(issues_sheet[,i]))}}" )
  
  # print count of types
  issues_sheet %>%
    count(type := !! sym(names(issues_sheet[,i]))) %>%
    print()
  
  # print(tibble(unique(issues_sheet[,i])))
  # cli::cli_inform( "{tibble(values = unique(issues_sheet[,i]))}" )
  
  cat("----------\n")
  
  for (j in seq(dim(unique(issues_sheet[,i]))[1])) {
    cli::cli_inform(
      "Showing the sheets for type: {.code {col_red(unique(issues_sheet[j,i]))}}")
    print( 
      issues_sheet[issues_sheet[,i] == unique(issues_sheet[,i])[j,][[1]],c(1,i)]
    )
    cat("\n")
  }
  
  if (i == ncol(issues_sheet)) cat("----------\n") 
}


for (j in seq(dim(unique(issues_sheet[,i]))[1])) {
  cli::cli_inform("Showing the sheets for type: {.code {unique(issues_sheet[j,i])}}")
  print( 
    issues_sheet[issues_sheet[,i] == unique(issues_sheet[,i])[j,][[1]],c(1,i)]
    )
  cat("\n\n")
}