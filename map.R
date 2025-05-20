library(tidyverse)
library(sf)
library(maptiles)
library(terra)
library(colorspace)


#read in location data
locations <- read_csv("data/locations.csv") %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

# get bounding box of main map to use in inset
locations_box <- st_bbox(c(xmin = 90, ymin = 41, xmax = 146, ymax = 56), crs = 4326) %>% 
  st_as_sfc() %>% 
  st_cast("LINESTRING")

# use labels or not?
# will always use no labels for inset map
mapchoice <- "CartoDB.PositronNoLabels"
#mapchoice <- "CartoDB.Positron"

# attribution string for basemap
credit <- get_credit(mapchoice)

# get basemap for inset plot
basemap_big <- get_tiles(st_bbox(c(xmin = 80, ymin = 20, xmax = 150, ymax = 60), crs = 4326), provider = "CartoDB.PositronNoLabels") %>% 
  as.data.frame(xy = TRUE) 
#convert to plotable raster
if ("red" %in% colnames(basemap_big)) {
  basemap_big <- basemap_big %>% 
    mutate(color = rgb(red, green, blue, maxColorValue = 255))
} else {
  basemap_big <- basemap_big %>% 
    mutate(color = rgb(lyr.1, lyr.2, lyr.3, maxColorValue = 255))
}

#get basemap for main map
basemap <- get_tiles(locations, provider = mapchoice) %>% 
  as.data.frame(xy = TRUE) 
#convert to plotable raster
if ("red" %in% colnames(basemap)) {
  basemap <- basemap %>% 
    mutate(color = rgb(red, green, blue, maxColorValue = 255)) %>% 
    mutate(color = if_else(color %in% c("#FAFAF8", "#D4DADC"), color, darken(color, 0.2)))
} else {
  basemap <- basemap %>% 
    mutate(color = rgb(lyr.1, lyr.2, lyr.3, maxColorValue = 255)) %>% 
    mutate(color = if_else(color %in% c("#FAFAF8", "#D4DADC"), color, darken(color, 0.2)))
}



# inset map - shouldn't need to be adjusted
inset_plot <- ggplot() +
  geom_raster(data = basemap_big, aes(x, y, fill = color)) + 
  scale_fill_identity() +
  geom_sf(data = locations_box) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_void() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5, linetype = "solid" ))
inset_grob <- ggplotGrob(inset_plot)

#main map
ggplot() +
  geom_raster(data = basemap, aes(x, y, fill = color)) + 
  scale_fill_identity() +
  # change size here to adjust circle size
  geom_sf(data = locations, aes(color = Species), size = 5) +
  # change colors here to adjust circle colors
  scale_color_manual(name = NULL, values = c("#228833", "#aa3377")) +
  # change breaks here to adjust axis labels
  scale_x_continuous(name = NULL, expand = c(0, 0), breaks = c(95, 105, 115, 125, 135, 145)) +
  scale_y_continuous(name = NULL, expand = c(0, 0), breaks = c(42, 46, 50, 54)) +
  # change x and y, size and label . size here to move attribution and change its size and border width
  geom_label(aes(x = 140, y = 41.3, label = credit), fill = "white",   color = "black", size = 2, label.size = 0.25, label.r = unit(0, "lines")) +
  # adjust position of inset map
  annotation_custom(grob = inset_grob, xmin = 91,  xmax = 101, ymin = 41.5, ymax = 46) +
  theme_bw() +
  # adjust legend position (0-1)
  theme(legend.position = "inside",
        legend.position.inside = c(0.3, 0.2),
        legend.background = element_rect(fill = "white", color = "black", linewidth = 0.2),
        legend.margin = margin(t = 5, r = 5, b = 5, l = 5)) 


"#FAFAF8" "#ACB5B6" "#A4AFB2" "#C5B9B7" "#C2A8A9" "#AFB3B4" "#D4DADC" "#B6BCBC" "#AFB5B4" "#B0B6B8" "#C4AEB0" "#C2ABAD"
[13] "#B5BBBD" "#B4B9B9" "#B6BCBA" "#A8AEB0" "#AAB1B2" "#C8B9B6" "#C1ADAF" "#C5B7B2" "#C6B3B3" "#C5AFAF" "#C7B7B5" "#C6B6B3"
[25] "#C1AAAB" "#BFA6A9" "#BFA4A6" "#C3ADAE" "#C1A6A9" "#C5B1B3" "#C4ADAD" "#B6BDBA" "#B7BCB9" "#C7B4B4" "#ABB2B3" "#A3AEB1"
[37] "#C5B2B0" "#A8AFB2" "#C5B0B2" "#C0A4A7" "#ACB5B8" "#AAB3B5" "#AFB7B7" "#A6B1B5" "#B1B6B8" "#ADB5B6" "#ADB3B4" "#C4B1B1"
[49] "#AAB4B7" "#B7BBB9" "#A9AFB0" "#BFA8AA" "#B5B9B9" "#A3AAAE" "#B4BFB6" "#B3B8B7" "#ABB3B7" "#ACB1B3" "#A8ADB1" "#B2B2B4"
[61] "#C3ADAD" "#AFB0B3" "#B3B8B8" "#AAB2B4" "#B4A9AC" "#A8ADAF" "#AFB5B6" "#A3ADB2" "#C1B8B8" "#B8BCBA" "#AFB3B3" "#AAADB0"
[73] "#BBABAC" "#B5B6B8" "#BFB7B5" "#BDA9AA" "#B7A7AA" "#B3A8AB" "#AFA8AC" "#AEA9AC" "#C2B6B4" "#AAAAAE" "#ADABAE" "#B5B0B2"
[85] "#B8B3B5" "#B8A6AA" "#B5A8AB" "#BDAAAC" "#B3ABAD" "#B6AAAD" "#B3ACB0" "#A9AAAE" "#B4AFB0" "#BDB5B5" "#B7A9AD" "#A6AFB3"
[97] "#B1AEB1" "#BCAAAD" "#AEABAE" "#BDA8AB" "#BAB7B8" "#AAB2B5" "#C0B7B5" "#B5B6BA" "#C0B7B7" "#B2A9AC" "#AAB1B5" "#B8B8B6"
[109] "#B1A8AB" "#C2B6B6" "#C2C2BA" "#B1ABAF" "#C5BFB8" "#C8BAB7" "#C4BCBF" "#A6AEB0" "#BBB3B5" "#A6AFB2" "#AFB7B9" "#B8A9AB"
[121] "#B2A8AC" "#B1B6B6" "#BABDBD" "#C0C0BC" "#B9C2B6" "#BCC3B8" "#A7B0B5" "#B3BABA" "#C5B0B0" "#C1A8AB" "#C4B3B3" "#C0A5A8"
[133] "#BABFBC" "#B7ABAD" "#C2AAAD" "#C0A7A9" "#BAC0BD" "#C2B7B7" "#BDB4B6" "#B4AAAB" "#B8A5A9" "#B9A6A9" "#BEA9AB" "#BCAAAB"
[145] "#A5AFB3" "#C0B9B7" "#B3A9AC" "#C1B8B6" "#C1B7B3" "#A8B2B4" "#A9AFAF" "#B7BBBB"

