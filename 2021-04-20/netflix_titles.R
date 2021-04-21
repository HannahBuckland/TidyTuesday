# TidyTuesday 19/04/2021
# Hannah Buckland
# Netflix titles - data from the Kaggle

library(tidytuesdayR)
library(tidyverse)
library(ggthemes)
library(patchwork)
library(extrafont)
library(ggtext)
library(ggfx)
library(lubridate)
library(Cairo)
library(grid)

tuesdata <- tidytuesdayR::tt_load('2021-04-20') # read in TidyTuesday Datat

# Convert date added to date using lubridate and filter from 2015 onwards
netflix <- tuesdata$netflix_titles %>%
  mutate(date_add_parse = mdy(date_added),
         date_add_parse = date(date_add_parse),
         date_month = floor_date(date_add_parse,unit = "month")) %>%
  filter(date_month >= "2015-01-01") 

# Count the number of titles added every month
monthly <- netflix %>%
  count(date_month) %>%
  mutate(label_pos = as.numeric(date_month))

# Adding month that Disney plus was launched in the US
disneyplus <- data.frame(date_month = "2019-11-01") %>%
  mutate(date_month = date(date_month),
         label_pos = as.numeric(date_month))
  

# Plotting -----------

# x axis breaks and labels
axisbreaks <- monthly$label_pos[seq(1,length(monthly$label_pos),12)]

axislabels <- data.frame(label = monthly$date_month[seq(1,length(monthly$date_month),12)]) %>%
  mutate(label = str_sub(label,end=4))

# colour information
bgcolour <- "grey10"
txtcolour <- "grey60"

# Positioning arrow
dis_arrow <- data.frame(x1 = disneyplus$label_pos+250, 
                        y1 = 0.8, 
                        x2 = disneyplus$label_pos+25, 
                        y2 = 1)

# horizontal tile plot
stripplt <- ggplot() + 
  as_reference(
    geom_text(aes(x=16500, y=1.2, label = 'NETFLIX'), hjust = 0,size = 30, family = "Bebas Neue"),
    id = "text"
  ) +
  with_blend(geom_tile(data = monthly, 
                       mapping = aes(x=date_month,
                                     y=1,
                                     fill = n),width = 30),
             bg_layer = "text",
             blend_type = 'out',
             id = "blended"
  ) +
  with_shadow("blended", sigma = 3, colour = "black") +
  geom_tile(data = disneyplus,
            mapping = aes(x=date_month,
                          y = 1), color = "white",
            fill = NA, width = 50, height = 1.1, linetype = 2) +
  geom_curve(data = dis_arrow,
             mapping = aes(x=x1,y=y1,xend=x2,yend=y2),
             curvature = 0.3,
             colour = txtcolour,
             arrow = arrow(length=unit(2,"mm"))) +
  annotate(geom = "text",
           x = dis_arrow$x1,
           y = dis_arrow$y1 - 0.05,
           label = "Disney+\nLaunched\nin the USA",
           colour = txtcolour,
           family = "Bebas Neue",
           vjust = 1) +
  scale_fill_gradient(low =  "#e50914", high ="#190103") +
  theme_void() +
  scale_x_continuous(breaks = axisbreaks, 
                     labels = axislabels$label, 
                     position = "bottom") +
  labs(caption = "Visualisation: Hannah Buckland | @HannahMBuckland \n Source: Data from Kaggle | #TidyTuesday \n") +
  guides(fill = guide_colourbar(
    barwidth = 15,
    barheight = 0.5,
    frame.colour = txtcolour,
    ticks = TRUE,
    ticks.colour = txtcolour,
    ticks.linewidth = 1,
    title = "Number of Titles Added Per Month",
    title.hjust = 0.5,
    title.position = "bottom"
  )) +
  theme(axis.text.x = element_text(family = "Bebas Neue",
                                   color = txtcolour,
                                   size = 12),
        plot.background = element_rect(fill = bgcolour),
        legend.position = "bottom",
        legend.box.margin = margin(1,0,0,0,"cm"),
        text = element_text(family = "Bebas Neue",
                            color = txtcolour),
        plot.margin = margin(1,0.5,0,0.5,"cm"),
        plot.caption = element_text(family = "Raleway", color = txtcolour, 
                                    size = 8, hjust = 1,
                                    margin = margin(t = 30)))

stripplt 

#save as Cairo png (anti alias)
ggsave("2021-04-20/netflixdisney.png", stripplt, width = 8, height = 5, dpi = 300, type = "cairo-png")

