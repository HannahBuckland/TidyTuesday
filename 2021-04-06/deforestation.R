# TidyTuesday 06/04/2021
# Hannah Buckland
# Our Wold in Data - deforestation data, visualisations based on articles by Hannah Ritchie and Max Roser

library(tidytuesdayR)
library(tidyverse)
library(ggthemes)
library(patchwork)
library(extrafont)
library(ggtext)
library(maptools)


tuesdata <- tidytuesdayR::tt_load('2021-04-06')

forestdata <- tuesdata$forest # area for net change in area of forest cover

breaks <- c(-2000000, -400000, -200000, -100000, 0, 100000, 200000, 400000, 2000000) # breaks to match OurData plots

# Data filtering and renaming for mapping
netchange <- forestdata %>%
  filter(year %in% c("2010","2015")) %>%
  mutate(region = entity) %>%
  filter(region != "World" & region != "Gibraltar" & region != "Tokelau")%>%
  mutate(region = recode(region,
                         "Congo" = "Republic of Congo",
                         "French Guyana" = "French Guiana",
                         "United Kingdom" = "UK",
                         "United States" = "USA"))

# Binning net forest change and assiging bin references
netchange2010 <- netchange %>%
  filter(year == "2010") %>%
  mutate(net_forest_conversion_f = .bincode(net_forest_conversion,breaks=breaks),
         net_forest_conversion_f = paste0("bin_",net_forest_conversion_f))

netchange2015 <- netchange %>%
  filter(year == "2015") %>%
  mutate(net_forest_conversion_f = .bincode(net_forest_conversion,breaks=breaks),
         net_forest_conversion_f = paste0("bin_",net_forest_conversion_f))

# Filtering out for top 5 net gain and top 5 net loss countries 
ranknetchange2010 <- netchange2010 %>%
  filter(net_forest_conversion > 0 | net_forest_conversion < -0) %>%
  arrange(net_forest_conversion) %>%
  mutate(region = factor(region, levels = region)) %>%
  filter(row_number() > max(row_number()) - 5 | row_number() <= 5) %>%
  mutate(rank_net = rank(net_forest_conversion), # rank from grain to loss
         start_net = c(-5e5,-5e5,-5e5,-5e5,-5e5,5e5,5e5,5e5,5e5,5e5), #offset for plots
         end_net = start_net+net_forest_conversion)

ranknetchange2015 <- netchange2015 %>%
  filter(net_forest_conversion > 0 | net_forest_conversion < -0) %>%
  arrange(net_forest_conversion) %>%
  mutate(region = factor(region, levels = region)) %>%
  filter(row_number() > max(row_number()) - 5 | row_number() <= 5) %>%
  mutate(rank_net = rank(net_forest_conversion),
         start_net = c(-5e5,-5e5,-5e5,-5e5,-5e5,5e5,5e5,5e5,5e5,5e5),
         end_net = start_net+net_forest_conversion)

# Read in world map data
world <- map_data("world")
world_filter <- world %>%
  filter(region != "Antarctica") 

# Join forest data with map data by region
net2010_change_map <- inner_join(world_filter, netchange2010, by = "region")
net2015_change_map <- inner_join(world_filter, netchange2015, by = "region")

rank2010_change_map <- inner_join(world_filter, ranknetchange2010, by = "region")
rank2015_change_map <- inner_join(world_filter, ranknetchange2015, by = "region")


# Plotting information ------------

labsbar <- data.frame(x=c(-1.5e6,-1.5e6,1.5e6,1.5e6),
                      y=c(10,8,5,3),
                      labels = c("TOP 5 COUNTRIES <br> WITH NET FOREST",
                                 "<span style='font-size:50pt;color:#4d908e'>GAIN</span> ",
                                 "TOP 5 COUNTRIES <br> WITH NET FOREST",
                                 "<span style='font-size:50pt;color:#f94144'>LOSS</span>"),
                      color = c("bin_8","bin_7","bin_2","bin_1"))

bgcolour <-  "#131416"

colourscale <- c(bin_1="#f94144",
                 bin_2="#f3722c",
                 bin_3="#f8961e",
                 bin_4="#f9c74f",
                 bin_5="#90be6d",
                 bin_6="#43aa8b",
                 bin_7="#4d908e",
                 bin_8="#577590")

scale_labs = c("-400 kha", 
               "-200 kha", 
               "-100 kha", 
               "0 ha", 
               "+100 kha", 
               "+200 kha", 
               "+400 kha",
               "+600 kha") 

# Manual design of legend courtesy of cnicault (Christophe Nicault @cnicault on Twitter)

legend <- tibble(level = c("bin_1", "bin_2", "bin_3", "bin_4", "bin_5", "bin_6", "bin_7", "bin_8"),
                 ymin = c(-400, -200, -100, 0, 0, 100, 200, 400),
                 ymax = c(-600, -400, -200, -100, 100, 200, 400, 600))

legend_txt <- legend %>%
  distinct(ymin) %>%
  mutate(offset = row_number() %% 2)

legend_plt <- legend %>%
  ggplot() +
  geom_rect(aes(xmin = 0, 
                xmax = 1, 
                ymin = ymin, 
                ymax = ymax, 
                fill = level), 
            color = bgcolour) +
  geom_text(data = legend_txt, 
            mapping = aes(x = 2 + offset, 
                          y = ymin, 
                          label = glue::glue("{scales::comma(ymin)} kha")), 
            vjust = 0, size = 4, colour = "white", family = "Oswald")+
  geom_segment(data = legend_txt, 
               mapping =aes(x = 1, 
                            xend = 1.5 +offset, 
                            y = ymin, 
                            yend = ymin), 
               linetype = "12", color = "white") +
  scale_fill_manual(values = colourscale) +
  scale_x_continuous(limits = c(-1.7, 4)) +
  coord_flip()+
  guides(fill = FALSE) +
  theme_void()


# Set base theme

theme_set(theme(
  panel.background = element_rect(
    fill = NA, 
    colour = NA),
  legend.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  plot.background = element_rect(
    colour = bgcolour,
    fill = bgcolour),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  axis.title = element_blank(),
  legend.position = "none",
  plot.margin = margin(0.5,0.5,0.5,0.5,"cm")))


# Individual plots 

bars2010 <- ggplot(data = ranknetchange2010) +
  geom_segment(mapping = aes(x = start_net, 
                             xend = end_net,
                             y = rank_net, 
                             yend = rank_net,
                             color = net_forest_conversion_f),
               size = 7) +
  scale_x_continuous(limits = c(-2.5e6,2.5e6))+
  scale_colour_manual(values=colourscale) +
  geom_richtext(mapping = aes(x=0,
                              y=rank_net, 
                              label = region, 
                              color = net_forest_conversion_f),
                hjust=0.5, 
                fill = NA, 
                label.color = NA, 
                family = "Simplifica", 
                size = 7) +
  geom_richtext(data = labsbar,
                mapping = aes(x=x,
                              y=y, 
                              label = labels,
                              color = color), 
                size = 7,
                label.color = NA,
                fill = NA,
                hjust = 0.5,
                vjust = 1,
                family = "Oswald") +
  ylab("2010") +
  theme(axis.title.y = element_text(colour = "white",
                                    size = 35,
                                    family = "Oswald"))

bars2015 <- ggplot(data = ranknetchange2015) +
  geom_segment(mapping = aes(x = start_net, 
                             xend = end_net,
                             y = rank_net, 
                             yend = rank_net,
                             color = net_forest_conversion_f),
               size = 7) +
  scale_x_continuous(limits = c(-2.5e6,2.5e6))+
  scale_colour_manual(values=colourscale) +
  geom_richtext(mapping = aes(x=0,
                              y=rank_net, 
                              label = region, 
                              color = net_forest_conversion_f),
                hjust=0.5, 
                fill = NA, 
                label.color = NA, 
                family = "Simplifica", 
                size = 7) +
  geom_richtext(data = labsbar,
                mapping = aes(x=x,
                              y=y, 
                              label = labels,
                              color = color), 
                size = 7,
                label.color = NA,
                fill = NA,
                hjust = 0.5,
                vjust = 1,
                family = "Oswald") +
  ylab("2015") +
  theme(axis.title.y = element_text(colour = "white",
                                    size = 35,
                                    family = "Oswald"))

world_base <- ggplot() +
  geom_polygon(data = world_filter, 
               mapping = aes(x = long, y = lat, group = group), 
               fill = "gray90", 
               color = "gray90", 
               size = .1) +
  theme(plot.margin = margin(1,1,1,1,"cm"))

map2010 <- world_base +
  geom_polygon(data = net2010_change_map, 
               mapping = 
                 aes(x = long, 
                     y = lat, 
                     group = group, 
                     fill = net_forest_conversion_f), 
               color = bgcolour, alpha = 0.5,
               size = .1) +
  geom_polygon(data = rank2010_change_map, 
               mapping = 
                 aes(x = long, 
                     y = lat, 
                     group = group, 
                     fill = net_forest_conversion_f), 
               color = bgcolour,
               size = 0.5) +
  scale_fill_manual(values=colourscale, labels = scale_labs) +
  coord_fixed(1.3) 

map2015 <- world_base +
  geom_polygon(data = net2015_change_map, 
               mapping = 
                 aes(x = long, 
                     y = lat, 
                     group = group, 
                     fill = net_forest_conversion_f), 
               color = "black", alpha = 0.7, 
               size = .1) +
  geom_polygon(data = rank2015_change_map, 
               mapping = 
                 aes(x = long, 
                     y = lat, 
                     group = group, 
                     fill = net_forest_conversion_f), 
               color = bgcolour,
               size = 0.5) +
  scale_fill_manual(values=colourscale) +
  coord_fixed(1.3)


# Patchwork plots together for final visualisation

forestplot <- bars2010 + (map2010 + inset_element(legend_plt, 0.1,-0.25,0.9,0.05, align_to = "panel") )+ bars2015 + map2015 +
  plot_layout(ncol = 2, widths = c(1,1.2)) +
  plot_annotation(
    caption = "Visualisation: Hannah Buckland | @HannahMBuckland \n Source: Our World In Data, by Hannah Ritchie and Max Roser | #TidyTuesday \n",
    theme = theme(plot.caption = element_text(family = "Courier", 
                                              size = 10, 
                                              color = "lightgrey", 
                                              hjust = 1)))
forestplot

ggsave("2021-04-06/deforestation_plot.png", forestplot, width = 15, height = 9)


