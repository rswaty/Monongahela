
library(tidyverse)

bps_scls_3 <- read.csv("data/bpsScls3.csv") %>%
  group_by(Var1) %>%
  mutate(total.count = sum(Freq)) %>%
  ungroup() %>%
  dplyr::filter(dense_rank(desc(total.count)) < 7) %>%
  dplyr::select(c("BpS_Name", "refLabel",  "currentPercent", "refPercent")) %>%
  pivot_longer(
    cols = c(`refPercent`, `currentPercent`), 
    names_to = "refCur", 
    values_to = "Percent"
  )

# order classes
bps_scls_3$refLabel <- factor(bps_scls_3$refLabel, levels= c(
  'Developed',
  'Agriculture',
  'Uncharacteristic exotic',
  'Uncharacteristic native',
  'E-Mature closed canopy deciduous',
  'E-Mature closed canopy deciduous/conifers',
  'E-Mixed mesophytic forests',
  'D-Mature open canopy deciduous',
  'D-Oak and pine forest',
  'C-Med. height open canopy deciduous',
  'C-Oak savanna and woodlands',
  'B-Med. height closed canopy deciduous',
  'B-Oak regeneration',
  'A-Oak regeneration',
  'A-Herbaceous'
))

# reverse
factor(bps_scls_3$refLabel, levels = rev(levels(bps_scls_3$refLabel)))


sclasplot <-
  ggplot(bps_scls_3, aes(fill=factor(refCur), y=Percent, x=refLabel)) + 
  geom_col(width = 0.8, position = position_dodge()) +
  coord_flip() +
  scale_x_discrete(limits = (levels(bps_scls_3$refLabel))) +
  labs(
    title = "Succession Classes past and present",
    subtitle = "All possible succession class labels listed. Not all succession classes present in all BpSs",
    caption = "\nData from landfire.gov.",
    x = "",
    y = "Percent")+
  theme_minimal(base_size = 14)+
  theme(plot.caption = element_text(hjust = 0, face= "italic"), #Default is hjust=1
        plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
        plot.caption.position =  "plot") +
  scale_fill_manual(values = c("#3d4740", "#32a852" ), # present (grey), historical (green)
                    name = " ", 
                    labels = c("Present",
                               "Past")) +
  theme(panel.spacing = unit(.05, "lines"))

sclasplot

sclasplotDMO <- sclasplot %+% subset(bps_scls_3, BpS_Name %in% c("Northeastern Interior Dry-Mesic Oak Forest"))


sclasplotDMO







