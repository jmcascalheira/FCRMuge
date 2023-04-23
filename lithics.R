lithics <- read.csv("lithics.csv", sep = ";")


lithics_counts <- lithics %>% 
  filter(CLASS %in% c("Core", "CoreFrag", "Flake", "FlakeFrag", "ElongBlank", "ElongBalnFrag", "RetouchedPiece", "RetouchedPieceFrag")) %>% 
  group_by(LAYER, CLASS) %>% 
  summarise(n = n()) %>%
  mutate(freq = (n / sum(n))*100)

ggplot(data=lithics_counts, aes(x=LAYER, y=n, fill=CLASS)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = mycolors) +
  theme_minimal() +
  theme(text=element_text(size=30), axis.text.x = element_text(angle = 45, hjust=1)) +
  xlab("Level")


lithics %>% 
  ggplot(aes(x = reorder(Level, -n), y = n, fill = Level)) +
  geom_bar(stat = "identity", position=position_dodge(), show.legend = FALSE) +
  scale_fill_manual(values = mycolors) +
  theme_minimal() +
  theme(text=element_text(size=30), axis.text.x = element_text(angle = 45, hjust=1)) +
  xlab("Level")




#### Raw Materials

material_counts <- lithics %>% 
  filter(CLASS %in% c("Core", "CoreFrag", "Flake", "FlakeFrag", "ElongBlank", "ElongBalnFrag", "RetouchedPiece", "RetouchedPieceFrag")) %>% 
  group_by(LAYER, RAWMATERIAL) %>% 
  summarise(n = n()) %>%
  mutate(freq = (n / sum(n))*100)


ggplot(data=material_counts, aes(x=LAYER, y=freq, fill=RAWMATERIAL)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = mycolors) +
  theme_minimal() +
  theme(text=element_text(size=30), axis.text.x = element_text(angle = 45, hjust=1)) +
  xlab("Level")



## Quartz quality

quartz_counts <- lithics %>% 
  filter(CLASS %in% c("Core", "CoreFrag", "Flake", "FlakeFrag", "ElongBlank", "ElongBalnFrag", "RetouchedPiece", "RetouchedPieceFrag")) %>% 
  group_by(LAYER, QUARTZQUALITY) %>% 
  summarise(n = n()) %>%
  mutate(freq = (n / sum(n))*100)


ggplot(data=quartz_counts, aes(x=LAYER, y=freq, fill=QUARTZQUALITY)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = mycolors) +
  theme_minimal() +
  theme(text=element_text(size=30), axis.text.x = element_text(angle = 45, hjust=1)) +
  xlab("Level")



## Cortex

cortex_counts <- lithics %>% 
  filter(CLASS %in% c("Core", "CoreFrag", "Flake", "FlakeFrag", "ElongBlank", "ElongBalnFrag", "RetouchedPiece", "RetouchedPieceFrag")) %>% 
  group_by(LAYER, CORTEX) %>% 
  summarise(n = n()) %>%
  mutate(freq = (n / sum(n))*100)


ggplot(data=cortex_counts, aes(x=LAYER, y=freq, fill=CORTEX)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = mycolors) +
  theme_minimal() +
  theme(text=element_text(size=30), axis.text.x = element_text(angle = 45, hjust=1)) +
  xlab("Level")



