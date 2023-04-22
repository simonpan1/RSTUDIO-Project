# Test read_html

#install.packages("rvest")
#install.packages("tidyverse")
#install.packages("stringr")
#install.packages("purrr")
#install.packages("dplyr")
#install.packages("tibble")
#install.packages("tidyr")
#install.packages("data.table")
#install.packages("readxl")
#install.packages("openxlsx")
#install.packages("reshape2")

library(openxlsx)
library(readxl)
library(tibble)
library(dplyr)
library(purrr)
library(tidyr)
library(data.table)
library(stringr)
library(rvest)
library(tidyverse)
library(reshape2)


x <- "C:/Users/Simon.Pan/Desktop/Test Comparing Tables/IMPORTANT URGENT/EST Country W26 NEW.html"

############################################################################ DIMENSIONS FUNCTION TO GET TOTAL BASE

final_base_dimensions <- function(x) {
  page <- read_html(x)
  # TEST WITH BASE
  
  axis_base <- page %>%
    html_elements(".Cell.CellTop.CellItemCount.CellElementBase") %>%
    html_text2()
  # CORRECT for Table label
  
  label <- page %>%
    html_elements(".AxisLabelHorizontalSide") %>%
    html_text2()
  
  # CREATE TIBBLE WITH TABLE LABEL + BASE
  base_label <- tibble(label, axis_base)
  base_label
  # CORRECT BASES + TABLE LABEL
  
  base_label$axis_base[base_label$axis_base == "-"] <- NA
  
  base_label <- base_label %>% drop_na()
  
  base_label
}

# TESTING FUNCTION
base_label <- final_base_dimensions(x)

############################################################################ DIMENSIONS FUNCTION TO GET CELL PERCENTAGES

final_cell_dimensions <- function(x) {
  page <- read_html(x)
  # CORRECT for axis percent
  
  axis_percent <- page %>%
    html_elements(".Cell.CellBottom.CellItemColPercent.CellElementCategory") %>%
    html_text2()
  
  # CORRECT for axis label
  axis_label <- page %>%
    html_elements(".Axis.AxisElementSide.AxisElementCategory") %>%
    html_text2()
  
  # CREATE TIBBLE WITH AXIS PERCENT + AXIS LABEL
  percent_label <- tibble(axis_label, axis_percent)

  # CORRECT PERCENT + AXIS_LABEL
  
  percent_label$axis_percent[percent_label$axis_percent == "-"] <- NA
  
  percent_label <- percent_label %>% drop_na()
  
  percent_label$axis_percent[percent_label$axis_percent == "*"] <- 0
  
  test1 <- gsub("%", "", percent_label$axis_percent)
  
  test <- tibble(percent_label,test1)
  final_percent_label <- test[,-2]
  
  final_percent_label
}

# TESTING FUNCTION
final_percent_label <- final_cell_dimensions(x)



# specifying the path name (NEED TO CREATE ESTONIA TABLES ASAP!)
y <- "C:/Users/Simon.Pan/Desktop/Test Comparing Tables/IMPORTANT URGENT/EST Filter W26 NEW.xlsx"

############################################################################ EXCEL FUNCTION TO GET TOTAL BASES

final_base_excel <- function(y) {
  # READING THE EXCEL CORRECT
  test_read <- excel_sheets(path = y)
  
  list_all <- lapply(test_read, function(x) read_excel(y, skip = 7, sheet = x))
  
  data <- rbindlist(list_all, fill = TRUE)

  # GET BASE FOR EXCEL
  
  filter_base <- data %>%
    filter(data[,1] == "Total (qualified)")
  filter_base
  
  # FINAL BASE VALUES FOR EXCEL CORRECT! BASES CORRECT FOR GRIDS
  
  excel_base <- filter_base[,3]
  
  excel_base[excel_base == 0] <- NA
  
  excel_base <- tibble(excel_base)
  
  final_excel_base <- excel_base %>% drop_na()
  
  # COMPARING TOTAL BASES CORRECT!
  
  test <- final_excel_base[1:172,]
  
  test1 <- tibble(base_label$axis_base)
  test2 <- test1[1:172,]
  final_data <- bind_cols(test, test2)
  
  final_data$test <- ifelse(final_data$"...1" == final_data$"base_label$axis_base", "TRUE", "FALSE")
  output <- final_data$test
  output
}

# TESTING FUNCTION
final_base_excel(y)


############################################################################ EXCEL FUNCTION TO GET CELL PERCENTAGES

final_cell_excel <- function(y) {
  # READING THE EXCEL CORRECT
  test_read <- excel_sheets(path = y)
  
  list_all <- lapply(test_read, function(x) read_excel(y, skip = 7, sheet = x))
  
  data <- rbindlist(list_all, fill = TRUE)
  
  # GET CELL PERCENTAGES FOR EXCEL
  filter_excel <- data %>%
    filter(!(data[,1] == "Total (qualified)"))
  
  # FINAL PERCENT VALUES FOR EXCEL CORRECT!
  
  excel_percent <- filter_excel[,2]
  
  excel_percent <- tibble(excel_percent)
  
  final_excel_percent <- excel_percent %>% drop_na()
  
  # CLEAN PERCENTAGES FOR EXCEL!
  final_excel_percent_1 <- as.numeric(final_excel_percent$...2) * 100
  
  final_excel_percent_2 <- round(final_excel_percent_1,0)
  
  final_excel_percent_2[final_excel_percent_2 == 0] <- NA
  
  final_final_excel_percent <- tibble(final_excel_percent_2)
  
  final_final_excel_percent <- final_final_excel_percent %>% drop_na()
  
  # COMPARING CELL PERCENTAGES CORRECT!
  lol1 <- tibble(final_percent_label$test1)
  head <- lol1[1:1225,]
  
  head1 <- final_final_excel_percent[1:1225,]
  
  percent <- bind_cols(head, head1)
  
  # EXCEL PERCENT MATCHES
  percent$test <- ifelse(percent$"final_percent_label$test1" == percent$"final_excel_percent_2", "TRUE", "FALSE")
  
  other_output <- percent$test
  other_output
}


# TESTING FUNCTION
final_cell_excel(y)












# CORRECT FOR AXIS VALUE
#axis_value <- page %>%
#  html_elements(".Cell.CellTop.CellItemCount.CellElementCategory") %>%
#  html_text2() %>%
# parse_number()
#axis_vaue

# CORRECT FOR MEAN FOR MEASURES
#axis_mean <- page %>% 
#html_elements(".Cell.CellTop.CellItemCount.CellElementMean") %>%
#  html_text2()

# INVESTIGATE GRID HERE
#
#
#
#list_all[[10]]
#
#test_final <- melt(list_all[[10]], id = "...1", value.name = ("count"), variable.name = "percent")
#
# Filter grid for percentages
#grid_final_final <- test_final %>%
#  filter(test_final[,3] < 1)
#
#final_col <- grid_final_final %>% dcast(percent ~ fct_inorder(...1), value.var = "count")
#
#one_col <- data.frame(unlist(final_col))
#
# filter off percent / add in total bases instead
#
#one_col_final <- one_col %>% 
#  filter(one_col[,1] < 1)
#
#value_percent_final <- tibble(...1 = grid_final_final[,1], `%` = one_col_final[,1], `123` = 1)
#
#grid_final_final <- test_final %>%
#  filter(test_final[,3] > 1)
#
#grid_final_final[,3]
#grid_final_sum <- sum(grid_final_final[,3])
#
#excel_total_base <- union(grid_final_final[,3], grid_final_sum)
#
#value_base_final <- tibble(...1 = "Total (qualified)", `%` = 1, `123` = excel_total_base)
#
# COMBINE BOTH TIBBLES AND ASSIGN INDEX TO LIST
#list_all[[10]] <- bind_rows(value_base_final, value_percent_final)
#list_all[[10]]
#
# FINISH INVESTIGATION HERE
#
#

