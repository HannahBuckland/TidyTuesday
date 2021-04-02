# TidyTuesday 30/03/2021
# Hannah Buckland
# Naked Truth - makeup shades from The Pudding article by Ofunne Amaka and Amber Thomas

library(tidytuesdayR)
library(tidyverse)
library(ggthemes)
library(stringr)
library(cowplot)
library(patchwork)
library(extrafont)
library(ggtext)

tuesdata <- tidytuesdayR::tt_load('2021-03-30')

allshades <- tuesdata$allShades
allcategories <- tuesdata$allCategories

# Get colours for continuous palette to use in plots ------

darkest <- allshades %>%
  filter(lightness == min(lightness)) %>%
  select("hex")

lightness_min <- allshades %>%
  filter(lightness == min(lightness)) %>%
  select("lightness")

lightest <- allshades %>%
  filter(lightness > 0.96) %>%
  filter(sat == max(sat)) %>%
  filter(hue == min(hue)) %>%
  select("hex")

avlightness <- mean(allshades$lightness)

midlight <- allshades %>%
  filter(lightness < avlightness + 0.001 & lightness > avlightness - 0.001 ) %>%
  filter(sat == max(sat)) %>%
  select("hex")

# Filtering data ----------
# Filter shades data down to only include colour data
lightdata <- allshades %>%
  select(c("hex","hue","sat","lightness"))

# Filter category data down to look at naming trends
fooddata <- allcategories %>%
  select(c("categories","lightness")) %>%
  filter(str_detect(categories, "food"))

drinkdata <- allcategories %>%
  select(c("categories","lightness")) %>%
  filter(str_detect(categories, "drink"))

complimentdata <- allcategories %>%
  select(c("categories","lightness")) %>%
  filter(str_detect(categories, "compliment"))

skindata <- allcategories %>%
  select(c("categories","lightness")) %>%
  filter(str_detect(categories, "skin"))

uniformdata <- data.frame(lightness=runif(n=10000,
                                          min = lightness_min$lightness, 
                                          max = 1))

# Function to get cumulative distributions of lightness data
f <- function(data,breaks){
  
  counts <- hist(data$lightness,breaks = breaks,plot = F)
  
  df <- data.frame(frac = counts$counts/sum(counts$counts))
  df$ymax <- cumsum(df$frac)
  df$ymin <- c(0,head(df$ymax,n=-1))
  df$minlightness <- head(breaks, n=-1)
  
  return(df)
}

breaks <- seq(0,1,length.out = 20) # Set breaks along lightness scale

categories <- list(uniform = uniformdata,
                   allshades = lightdata,
                   food=fooddata,drink=drinkdata,
                   skin=skindata,
                   compliment=complimentdata) 

bincat <- lapply(categories,FUN=f,breaks=breaks)

bincatun <- bind_rows(bincat,.id="id") 

# Set theme for plotting ---------

theme_set(theme(
  text = element_text(size = 14, 
                      family = "Raleway", 
                      colour = "white"),
  strip.text = element_text(family = "Raleway", 
                            margin = margin(.1, 0, .1, 0, "cm"),
                            colour = "white", 
                            face = "bold", 
                            size = 12),
  strip.background = element_rect(colour = NA,
                                  fill = NA),
  panel.background = element_rect(
    fill = NA, 
    colour = NA),
  legend.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  plot.background = element_rect(
    colour = "lightblue4",
    fill = "lightblue4"),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  axis.title = element_blank(),
  legend.position = "none",
  plot.margin = margin(0,0.2,0.2,0.2,"cm")))

#  Set up data frame of donut plot labels
lab_text <- data.frame(
  label = c(round(mean(uniformdata$lightness),2),round(mean(lightdata$lightness),2),round(mean(complimentdata$lightness),2), round(mean(drinkdata$lightness),2), round(mean(fooddata$lightness),2), round(mean(skindata$lightness),2)),
  id   = c("uniform","allshades","compliment","drink", "food", "skin"),
  x = rep(2,6),
  y = rep(0,6),
  ymax = rep(Inf,6),
  ymin = rep(Inf,6),
  xmax = rep(4,6),
  xmin = rep(3,6),
  minlightness = rep(0.5,6))

# Create donut plots -------

lightness_donut <- ggplot(data = subset(bincatun,id =="uniform" | id =="allshades"), 
                          mapping = aes(ymax=ymax,ymin=ymin,xmax=4,xmin=3,fill = minlightness)) + 
  geom_rect(colour = "white", 
            size = 0.2) +
  geom_text(data = subset(lab_text,id =="uniform" | id =="allshades"), 
            size = 7, 
            mapping = aes(x = x, y = y, label = label),
            family = "Raleway", 
            fontface = "bold", 
            colour = "white") +
  scale_fill_gradientn(colours=c(darkest$hex, midlight$hex,high=lightest$hex),
                       limits = c(lightness_min$lightness,1)) +
  coord_polar(theta = "y") +
  xlim(c(2,4)) + 
  theme(plot.margin = margin(0,0,0,0,"cm"))

cat_donut <- ggplot(data = subset(bincatun,id !="allshades" & id !="uniform"), 
                    mapping = aes(ymax=ymax,ymin=ymin,xmax=4,xmin=3, fill = minlightness)) + 
  geom_rect(colour = "white", 
            size = 0.3) +
  geom_text(data =subset(lab_text,id !="allshades" & id !="uniform"), 
            size = 4, 
            mapping = aes(x = x, y = y, label = label),
            family = "Raleway", 
            fontface = "bold", 
            colour = "white") +
  scale_fill_gradientn(colours=c(darkest$hex, midlight$hex,high=lightest$hex), 
                       limits = c(lightness_min$lightness,1)) +
  coord_polar(theta = "y") +
  xlim(c(2,4)) 

row1_donuts <- lightness_donut + facet_wrap(~id,nrow=1)
row2_donuts <- cat_donut + facet_wrap(~id,nrow=1)

# Final plot ----------

makeupplot <- row1_donuts / row2_donuts +
  plot_layout(heights = c(1.5,1.4)) +
  plot_annotation(title = "\nNaming Bias in Makeup Shades",
                  subtitle = "\nThe names and shade data of 5000+ shades of foundation have been compiled by\nThe Pudding and assigned categories. This visualisation shows the proportions of light \nto dark shades in all products, the proportions if shades were uniformly distributed across \nthe lightness spectrum and the distribution per naming category. The number in the \ncentre of each donut plot represents the average lightness value per category.\n",
                  caption = "Visualisation: Hannah Buckland | @HannahMBuckland \n Source: The Pudding, Ofunne Amaka and Amber Thomas. | #TidyTuesday\n",
                  theme = theme(plot.title = element_text(size = 24, 
                                                          family = "Josefin Slab",
                                                          face = "bold",
                                                          hjust = 0),
                                plot.subtitle = element_text(hjust = 0,
                                                             family = "Raleway",
                                                             size = 10,
                                                             colour = lightest$hex),
                                plot.caption = element_text(family = "Courier", 
                                                            size = 10, 
                                                            color = lightest$hex, 
                                                            hjust = .5)))

ggsave("2021-03-30/beauty_bias_plot.png", makeupplot, width = 6, height = 7)

