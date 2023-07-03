library(tidyverse)
library(readxl)
#----------------------=====

read <- function(range, sheet){
  read_excel("Total tissue data .xlsx", range = range, sheet = sheet)
}

read_data <- function(sheet){
diet1 <- read("H1:M13", sheet)
diet3 <- read("H15:M27", sheet)
diet4 <- read("H29:M41", sheet)
diet5 <- read("H43:M55", sheet)
diet7 <- read("H57:M69", sheet)
diet8 <- read("H71:M83", sheet)
diet2 <- read("H85:M97", sheet)
diet6 <- read("H99:M111", sheet)

colnames(diet1) <- colnames(diet1)
colnames(diet2) <- colnames(diet1)
colnames(diet3) <- colnames(diet1)
colnames(diet4) <- colnames(diet1)
colnames(diet5) <- colnames(diet1)
colnames(diet6) <- colnames(diet1)
colnames(diet7) <- colnames(diet1)
colnames(diet8) <- colnames(diet1)


ileum <- dplyr::bind_rows(list(diet1=diet1,
                      diet2=diet2,
                      diet3=diet3,
                      diet4=diet4,
                      diet5=diet5,
                      diet6=diet6,
                      diet7=diet7,
                      diet8=diet8), .id = 'Diet') %>% 
  rename("Tissue" = `Diet 1`)

ileum %>% 
  separate(`Tissue`, into = c("Tissue", "id"), sep = "(?<=\\D)(?=\\d)", remove = FALSE) %>% 
  mutate(`Tissue` = str_trim(`Tissue`, side = "both"))
}


#----------------------=====

ileum <- read_data(2)
duodenum <- read_data(3)
leg <- read_data(4)
jejunum <- read_data(9)
