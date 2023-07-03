library(tidyverse)
library(readxl)
#----------------------=====

read <- function(range, sheet){
  read_excel("Total tissue data .xlsx", range = range, sheet = sheet)
}

read_data <- function(sheet,
                      range1 = "H1:M13",
                      range2 = "H15:M27",
                      range3 = "H29:M41",
                      range4 = "H43:M55",
                      range5 = "H57:M69",
                      range6 = "H71:M83",
                      range7 = "H85:M97",
                      range8 = "H99:M111"){
diet1 <- read(range1, sheet)
diet3 <- read(range2, sheet)
diet4 <- read(range3, sheet)
diet5 <- read(range4, sheet)
diet7 <- read(range5, sheet)
diet8 <- read(range6, sheet)
diet2 <- read(range7, sheet)
diet6 <- read(range8, sheet)

colnames(diet1) <- colnames(diet1)
colnames(diet2) <- colnames(diet1)
colnames(diet3) <- colnames(diet1)
colnames(diet4) <- colnames(diet1)
colnames(diet5) <- colnames(diet1)
colnames(diet6) <- colnames(diet1)
colnames(diet7) <- colnames(diet1)
colnames(diet8) <- colnames(diet1)


data <- dplyr::bind_rows(list(diet1=diet1,
                      diet2=diet2,
                      diet3=diet3,
                      diet4=diet4,
                      diet5=diet5,
                      diet6=diet6,
                      diet7=diet7,
                      diet8=diet8), .id = 'Diet') %>% 
  rename("Tissue" = `Diet 1`)

data %>% 
  separate(`Tissue`, into = c("Tissue", "id"), sep = "(?<=\\D)(?=\\d)", remove = FALSE) %>% 
  separate(id, into = c("individual", "rep"), sep = -1, remove = TRUE) %>% 
  mutate(`Tissue` = str_trim(`Tissue`, side = "both"))
}


#----------------------=====

ileum <- read_data(2)
ileum <- ileum %>% mutate(Tissue = "Ileum tissue")

duodenum <- read_data(3)
duodenum <- duodenum %>% mutate(Tissue = "Duodenal tissue")

leg <- read_data(4)
leg <- leg %>% mutate(Tissue = "Leg")

jejunum <- read_data(9)
jejunum <- jejunum %>% mutate(Tissue = "Jejunum tissue")

brain <-  read_data(5, "H1:M13",
                  "H16:M28",
                  "H31:M43",
                  "H46:M58",
                  "H61:M73",
                  "H76:M88",
                  "H90:M101",
                  "H103:M115")
brain <- brain %>% mutate(Tissue = "Brain")

kidney <- read_data(6, 
                    "I1:O13",
                    "I16:O28",
                    "I29:O41",
                    "I43:O55",
                    "I57:O68",
                    "I70:O82",
                    "I84:O96",
                    "I98:O110")
kidney <- kidney %>% mutate(Tissue = "Kidney")

liver <- read_data(7, "A2:F14",
                   "A16:F28",
                   "A31:F43",
                   "A45:F57",
                   "A59:F71",
                   "A73:F85",
                   "A87:F99",
                   "A101:F113")
liver<- liver %>% mutate(Tissue = "Liver")

breast <- read_data(8, "A2:F14",
                   "A16:F28",
                   "A30:F42",
                   "A44:F56",
                   "A58:F70",
                   "A72:F84",
                   "A86:F98",
                   "A100:F112")

breast <- breast %>% mutate(Tissue = "BM")

total_data <- dplyr::bind_rows(ileum, duodenum, leg, jejunum, brain, kidney, liver, breast)

data_long <- total_data %>% 
  select(-IP2) %>% 
  pivot_longer(IP3:IP6, names_to = "IP", values_to = "value")

#----------------------=====
# Analyse====

model <- lm(value ~ IP + Diet, data = data_long)



#log
data_long <- data_long %>% 
  mutate(log_value = log10(value+1))


model <- lm(log10(value+1) ~ IP + Diet, data = data_long)

ran_mod <- lmer(log10(value+1) ~ 1 + (1|individual/rep) + (1|Tissue), data = data_long)

ran_mod <- lmer(log_value ~ IP + Diet + (1|individual/rep) + (1|Tissue), data = data_long)

resid.mm <- simulateResiduals(ran_mod)

plot(resid.mm)

# scaled and centered

data_long <- data_long %>% 
  mutate(scaled_var = (log10(value+1) - mean(log10(value+1), na.rm = T)) / sd(log10(value+1), na.rm = T))

