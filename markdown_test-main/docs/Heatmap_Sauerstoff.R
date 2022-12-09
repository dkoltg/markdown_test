### Heatmap Sauerstoffgehalt in der Seemitte 

library(plyr)
library(dplyr)
library(ggplot2)
library(sp)
library(sf)
#library(rlang)
library(rgdal)
#library(leaflet)
library(ggrepel)
#library(raster)
library(tidyverse)
#library(maps)
library(stringr)
library(forecast)
library(lubridate)
library(viridis)
library(reshape2)
library(grid)
library(epitools)
library(foreach)
library(scales)
library(coda)
library(eeptools)


library(lubridate)
sort <- read.csv("S:/Daten/High Charts/Beispiel-Daten/seetiefe_ref.csv", header = TRUE, sep=";")

wasserqual <- read.csv("S:/Daten/High Charts/Beispiel-Daten/data_sauerstoff_see.csv", header = TRUE, sep=";") %>% 
  mutate(Datum=as.Date(Datum, format = "%d.%m.%Y")) %>% 
  left_join(sort, by=c("Seetiefe"="Seetiefe")) %>% 
  mutate(Seetiefe=as.character(Seetiefe)) %>% 
  arrange(sort)

# Heatmap einfach
plot.heatmap <- ggplot(wasserqual, aes(x=Datum, y=reorder(Seetiefe, sort), fill=Sauerstoffgehalt)) +
  geom_tile(colour="transparent") +
  scale_fill_gradient(low="#000099", high="#ff0000")  + theme_opts +
  theme(
    strip.text.x = element_text(
      size = 10, color = "black", face = "bold"
    ),
    strip.text.y = element_text(
      size = 10, color = "black", face = "bold"
    )
  ) + theme(strip.background =element_rect(fill="transparent")) + 
  theme(panel.spacing = unit(0, "lines")) +
  labs(title = "Sauerstoffverteilung in der Seemitte", 
       subtitle = "2012-2021",
       y="Seetiefe", x="Datum") 

plot.heatmap

# Heatmap mit Mittelwert
plot.heatmap2 <- ggplot(wasserqual, aes(x=Datum, y=reorder(Seetiefe, sort))) +
  geom_tile(aes(fill = Sauerstoffgehalt), colour = "transparent", width = 50, height = 2) +
  scale_fill_gradient2(low="#000099", mid="#b3ffe6", high="#ff0000")  + theme_opts +
  theme(
    strip.text.x = element_text(
      size = 10, color = "black", face = "bold"
    ),
    strip.text.y = element_text(
      size = 10, color = "black", face = "bold"
    )
  ) + theme(strip.background =element_rect(fill="transparent")) + 
  theme(panel.spacing = unit(0, "lines")) +
  labs(title = "Sauerstoffverteilung in der Seemitte", 
       subtitle = "2012-2021",
       y="Seetiefe", x="Datum") 
plot.heatmap2

# Heatmap mit spezifischer Farbpalette
plot.heatmap3 <- ggplot(wasserqual, aes(x=Datum, y=reorder(Seetiefe, sort))) +
  geom_tile(aes(fill = Sauerstoffgehalt), colour = "transparent", width = 50, height = 2) +
  scale_fill_gradientn(colours=c("#000099", "#1a1aff", "#00ccff",
                                 "#00ccff", "#ffe066", "#ff944d",
                                 "#ff6600", "#ff0000"),
                       values=rescale(c(0, 2, 4, 6, 8 , 10, 12, 14)),
                       guide="colorbar") + 
  theme(
    strip.text.x = element_text(
      size = 10, color = "black", face = "bold"
    ),
    strip.text.y = element_text(
      size = 10, color = "black", face = "bold"
    )
  ) + theme(strip.background =element_rect(fill="transparent")) + 
  theme(panel.spacing = unit(0, "lines")) +
  labs(title = "Sauerstoffverteilung in der Seemitte", 
       subtitle = "2012-2021",
       y="Seetiefe", x="Datum") 
plot.heatmap3

ggplotly(plot.heatmap3)


