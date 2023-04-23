# Field data

library(dplyr)
library(stringr)
library(tidyr)
library(readr)
library(ggplot2)
library(corrplot)
library(tibble)
library(RColorBrewer)
library(ggpubr)

options(scipen=999)

# Load data
FCR <- read.csv("FCR.csv", sep = ";")
context <- read.csv("context.csv")
xyz <- read.csv("xyz.csv")

cols_to_concat <- c("Unit", "ID")

context$ID <- str_squish(context$ID)
xyz$ID <- str_squish(xyz$ID)

context <- context %>%
  unite_(col='id', cols_to_concat, sep="-", remove=FALSE) %>%
  select(id, Level, Code, Excavator) %>%
  distinct(id, .keep_all = TRUE)

xyz <- xyz %>%
  rename(Unit = UNIT) %>%
  unite_(col='id', cols_to_concat, sep="-", remove=FALSE) %>%
  filter(Suffix <= 1) %>%
  select(id, X, Y, Z, Suffix)


# Join tables

field_data <- right_join(xyz, context, by = "id")

FCR <- FCR %>% 
  rename(id = ID)

data <- right_join(field_data, FCR, by = "id")


# Some tidying

data <- data %>% 
  filter(TYPE %in% c("FCR", "PEBBLE"), !is.na(Level)) %>% 
  mutate(Level = case_match(Level, "Pit_1" ~ "PIT1", "9a" ~ "9A", "Pit_1b" ~ "PIT1b", "Pit_1d" ~ "PIT1d", "2a" ~ "2A", "3b" ~ "3B", .default = Level))


# Analysis

type_count <- data %>% 
  group_by(TYPE) %>% 
  tally()

general <- data %>% 
  group_by(TYPE, Level) %>% 
  tally()

nb.cols <- 18
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)


general %>% 
  ggplot(aes(x = reorder(Level, -n), y = n, fill = Level)) +
  geom_bar(stat = "identity", position=position_dodge(), show.legend = FALSE) +
  scale_fill_manual(values = mycolors) +
  theme_minimal() +
  theme(text=element_text(size=30), axis.text.x = element_text(angle = 45, hjust=1)) +
  xlab("Level")


general <- general %>% pivot_wider(names_from = TYPE, values_from = n)


data %>% 
  
  ggplot(aes(x = X, y = Z)) +
  geom_point()
  

data %>% 
  case_match(LEVEL) %>% 
  filter(X > 102, TYPE %in% c("FCR", "PEBBLE")) %>% 
  group_by(LEVEL, TYPE) %>% 
  tally()

  
  
  
# Stats FCR vs PEBBLES
  
library("gplots")
  
main_levels <- general %>% filter(PEBBLE > 25, Level %in% c("9", "PIT1"))

main_levels <- column_to_rownames(main_levels, var = "Level")


main_levels_balloon <- as.table(as.matrix(main_levels))
balloonplot(t(main_levels_balloon), main ="FCR", xlab ="", ylab="",
            label = FALSE, show.margins = T)

chisq <- chisq.test(main_levels)
chisq


corrplot(chisq$residuals, is.cor = FALSE, tl.srt = 45, tl.cex = 4)

round(chisq$p.value, digits = 4)



# Stats FRACTURE

fracture <- data %>%
  mutate_at(c('FRACTURE'), ~na_if(., '')) %>% 
  filter(!is.na(FRACTURE), Level %in% c("9", "PIT1")) %>% 
  group_by(FRACTURE, Level) %>% 
  tally()


fracture <- fracture %>%
#  filter(n > 10) %>% 
  pivot_wider(names_from = FRACTURE, values_from = n)


fracture <- fracture %>%
  na.omit()
  

fracture <- column_to_rownames(fracture, var = "Level")


fracture_balloon <- as.table(as.matrix(fracture))
balloonplot(t(fracture_balloon), main ="FCR", xlab ="", ylab="",
            label = FALSE, show.margins = T)

chisq <- chisq.test(fracture)
chisq


corrplot(chisq$residuals, is.cor = FALSE, number.cex = 2, tl.cex = 2)

round(chisq$p.value, digits = 4)


# fine crazing

crazing <- data %>%
  mutate_at(c('FINECRAZING'), ~na_if(., '')) %>% 
  filter(!is.na(FINECRAZING), Level %in% c("9", "PIT1")) %>% 
  group_by(FINECRAZING, Level) %>% 
  tally()

crazing <- crazing %>%
  #filter(n > 5) %>% 
  pivot_wider(names_from = FINECRAZING, values_from = n)

crazing <- crazing %>%
  na.omit()


crazing <- column_to_rownames(crazing, var = "Level")


crazing_balloon <- as.table(as.matrix(crazing))
balloonplot(t(crazing_balloon), main ="FCR", xlab ="", ylab="",
            label = FALSE, show.margins = T)

chisq <- chisq.test(crazing)
chisq


corrplot(chisq$residuals, is.cor = FALSE, number.cex = 2, tl.cex = 2)

round(chisq$p.value, digits = 4)


# Deep cracking


cracking <- data %>%
#  mutate_at(c('DEEPCRACKING'), ~na_if(., '')) %>% 
  filter(!is.na(DEEPCRACKING), Level %in% c("9", "PIT1")) %>% 
  group_by(DEEPCRACKING, Level) %>% 
  tally()

cracking <- cracking %>%
  #filter(n > 5) %>% 
  pivot_wider(names_from = DEEPCRACKING, values_from = n)


cracking <- column_to_rownames(cracking, var = "Level")


cracking_balloon <- as.table(as.matrix(cracking))
balloonplot(t(cracking_balloon), main ="FCR", xlab ="", ylab="",
            label = FALSE, show.margins = T)

chisq <- chisq.test(cracking)
chisq


corrplot(chisq$residuals, is.cor = FALSE, number.cex = 2, tl.cex = 2)

round(chisq$p.value, digits = 4)


## Weight

data %>%
  filter(!is.na(WEIGHT), Level %in% c("9", "PIT1"), TYPE == "PEBBLE", WEIGHT < 1000) %>% 
  group_by(Level) %>% 
  summarise(mean = median(WEIGHT), max = max(WEIGHT), min = min(WEIGHT), n = n())


weight <- data %>% 
  filter(!is.na(WEIGHT), Level %in% c("9", "PIT1"), WEIGHT < 1000)

ggplot(weight, aes(x=TYPE, y=WEIGHT, color=Level)) +
  geom_boxplot() +
  scale_fill_manual(values = mycolors) +
  theme_minimal() +
  theme(text=element_text(size=30), axis.text.x = element_text(angle = 45, hjust=1)) +
  xlab("Type")


p <- ggboxplot(weight, x = "Level", y = "WEIGHT",
          color = "Level", palette = "jco",
          add = "jitter",
          facet.by = "TYPE", short.panel.labs = FALSE)

p + stat_compare_means(paired = F, method = "t.test") +
  theme(text=element_text(size=30))



# Discoloration

color <- data %>%
  mutate(across(c("COLORCHANGES"), ~ifelse(.=="", NA, as.character(.)))) %>% 
  filter(!is.na(COLORCHANGES), Level %in% c("9", "PIT1")) %>% 
  group_by(COLORCHANGES, Level) %>% 
  tally()

color <- color %>%
  #filter(n > 5) %>% 
  pivot_wider(names_from = COLORCHANGES, values_from = n)


cracking <- column_to_rownames(cracking, var = "Level")


cracking_balloon <- as.table(as.matrix(cracking))
balloonplot(t(cracking_balloon), main ="FCR", xlab ="", ylab="",
            label = FALSE, show.margins = T)

chisq <- chisq.test(cracking)
chisq


corrplot(chisq$residuals, is.cor = FALSE, number.cex = 2, tl.cex = 2)

round(chisq$p.value, digits = 4)


# Carbon

carbon <- data %>%
  filter(!is.na(CARBONDEPOSITION), Level %in% c("9", "PIT1")) %>% 
  group_by(CARBONDEPOSITION, Level) %>% 
  tally()

carbon <- carbon %>%
  #filter(n > 5) %>% 
  pivot_wider(names_from = CARBONDEPOSITION, values_from = n)


carbon <- column_to_rownames(carbon, var = "Level")


chisq <- chisq.test(carbon)
chisq


corrplot(chisq$residuals, is.cor = FALSE, number.cex = 2, tl.cex = 2)

round(chisq$p.value, digits = 4)




# Concretions

concretions <- data %>%
  filter(!is.na(CONCRETIONS), Level %in% c("9", "PIT1")) %>% 
  group_by(CONCRETIONS, Level) %>% 
  tally()

concretions <- concretions %>%
  #filter(n > 5) %>% 
  pivot_wider(names_from = CONCRETIONS, values_from = n)


carbon <- column_to_rownames(carbon, var = "Level")


chisq <- chisq.test(carbon)
chisq


corrplot(chisq$residuals, is.cor = FALSE, number.cex = 2, tl.cex = 2)

round(chisq$p.value, digits = 4)




