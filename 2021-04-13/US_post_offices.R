# TidyTuesday 13/04/2021
# Hannah Buckland
# US Post Offices - Harvard Dataverse https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/NUKCNA

library(tidytuesdayR)
library(tidyverse)
library(ggthemes)
library(extrafont)
library(ggtext)
library(geojsonio)
library(rgdal)
library(rgeos)
library(mapproj)
library(usmap)

tuesdata <- tidytuesdayR::tt_load('2021-04-13')

postoffices <- tuesdata$post_offices

# USA hexagon shapefile found at: https://rud.is/b/2015/05/14/geojson-hexagonal-statebins-in-r/
map_hex <- geojson_read("2021-04-13/us_states_hexgrid.geojson",  what = "sp")

us_hexmap_buffer <-
  gBuffer(map_hex, width = -.2, byid = T) # buffer around hexagons

us_hexmap <- fortify(us_hexmap_buffer, region="iso3166_2") %>%
  filter(id != "DC") # not interesting in DC only has 1 entry

usstate_pop <- statepop %>%
  mutate(id = abbr)

# filter post offices to only include current ones (i.e. not discontinued)
currentPO <- postoffices %>%
  filter(established > 1639) %>%
  filter(is.na(discontinued)) %>%
  filter(state != "DC")

# count the number of POs in every state
countsPO <- currentPO %>%
  count(state) %>%
  mutate(id = state)

# get coordinates of the centre of each state hexagon for label positioning
centers <- cbind.data.frame(data.frame(gCentroid(map_hex, byid=TRUE), id=map_hex@data$iso3166_2)) %>%
  filter(id != "DC")

# join up data sources
us_hexmap <- inner_join(us_hexmap,usstate_pop, by = "id")
us_hexmap <- inner_join(us_hexmap,countsPO, by = "id")

# binning the post offices per people
breaks <- c(0,5000,10000,15000,20000,50000) #

us_hexmap_cat <- us_hexmap %>%
  mutate(PO_percapita = pop_2015/n,
         PO_percapita_bin = .bincode(PO_percapita,breaks=breaks),
         PO_percapita_bin = paste0("bin_",PO_percapita_bin),
         PO_percapita_bin = factor(PO_percapita_bin, levels = c("bin_1","bin_2","bin_3","bin_4","bin_5")))

# colour scale using roughly the USPS colours
colourscale <- c(bin_1="#004b87",
                 bin_2="#6289A3",
                 bin_3="white",
                 bin_4="#bfa3bc",
                 bin_5="#812b44")

# label the binned people per post office
scalelabels <- data.frame(id =seq(1,5)) %>%
  mutate(label = paste0(breaks[row_number()],"-",breaks[(row_number())+1])) %>%
  mutate(label = ifelse(label == "20000-50000", ">20000",label))

scalelabels <- c(bin_1 = scalelabels$label[1],
                 bin_2 = scalelabels$label[2],
                 bin_3 = scalelabels$label[3],
                 bin_4 = scalelabels$label[4],
                 bin_5 = scalelabels$label[5])

# find state with the lowest number of people served per PO
minimum_per <- us_hexmap_cat %>%
  filter(PO_percapita == min(PO_percapita)) %>%
  select("id","PO_percapita") %>%
  unique()

# find state with the highest number of people served per PO
maximum_per <- us_hexmap_cat %>%
  filter(PO_percapita == max(PO_percapita)) %>%
  select("id","PO_percapita") %>%
  unique()


# Plotting information 
plt_title <- data.frame(x = -105, y = 53.8,
                        label = "<b style='color:#f5f5f5;font-size:25pt;'>How long will you wait in line at the Post Office?</b><br><br>
      Each hexagon shows the number of people per post office in every state (excluding DC). You might expect the<br>fastest service in <b style='color:#004b87;font-size:14pt;'>North Dakota</b>, where each post office serves an average of 2000 people. In <b style='color:#812b44;font-size:14pt;'>Florida</b>, however,<br>you might be waiting a while as each post office serves on average > 40000 people.")

bgcolour <- "grey7"

# Final plot
hexplt <- ggplot(data=us_hexmap_cat) + 
  geom_polygon(data = us_hexmap_cat,
               aes(long, lat, 
                   group = id,
                   fill = PO_percapita_bin)) +
  geom_text(data = centers,aes(x=x, y=y, label=id), size =8, color = "grey7", family = "Oswald") +
  geom_richtext(data = plt_title,
                mapping = aes(x=x,y=y,label=label),
                family = "Oswald",
                label.color = NA,
                fill = NA,
                color = "white",
                size = 5) +
  scale_fill_manual(values = colourscale, labels = scalelabels, name = "People per Post Office") +
  coord_map() +
  theme_void() +
  labs(caption = "Visualisation: Hannah Buckland | @HannahMBuckland \n Source: Harvard Dataverse; Blevins, Helbock & Richard \"US Post Offices\"(2021) | #TidyTuesday \n") +
  theme(legend.position = c(0.9,0.15),
        legend.background = element_rect(fill = NA,
                                         colour = NA),
        legend.key.size = unit(0.8,"cm"),
        legend.margin = margin(0.4,0.4,0.4,0.4, "cm"),
        legend.text = element_text(family = "Averia Libre",
                                   size = 12),
        legend.title = element_text(family = "Averia Libre",
                                    size = 12),
        panel.background = element_rect(fill = bgcolour,
                                        colour = NA),
        plot.background = element_rect(fill = bgcolour,
                                       colour = NA),
        text = element_text(color = "white"),
        plot.margin = margin(0.5,0.5,0.5,0.5, unit = "cm"),
        plot.caption = element_text(family = "Averia Libre", color = "grey82", 
                                    size = 11, hjust = .5,
                                    margin = margin(b = 5, t = 20)))

hexplt 

ggsave("2021-04-13/post_offices_plot.png", hexplt, width = 15, height = 9)


