pacman::p_load(dplyr, tidyr, readxl, janitor)

# Load the cNMA data from an Excel file
#cNMA_data <- read_xlsx("CNMA Master Database 4-22-25_sbm.xlsx", sheet = "Master Database", range="C1:BZ410")
cNMA_data <- read_xlsx("CNMA Master Database 4-22-25_sbm.xlsx", sheet = "Master Database")

str(cNMA_data)

class(cNMA_data$study_id)
class(cNMA_data$contrast_id)
class(cNMA_data$es_id)

options(max.print = 1000000)
tabyl(cNMA_data$study_id)
tabyl(cNMA_data$contrast_id)
tabyl(cNMA_data$es_id)