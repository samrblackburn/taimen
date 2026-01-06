map_targets <- list(
  # get list of population locations to map
  tar_target(locations_file, "data/locations.csv", format = "file"),
  tar_target(
    locations,
    read_csv(locations_file) %>%
      st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
  ),

  # get bounding box of main map to use on inset map
  tar_target(
    locations_box,
    st_bbox(c(xmin = 90, ymin = 41, xmax = 146, ymax = 56), crs = 4326) %>%
      st_as_sfc() %>%
      st_cast("LINESTRING")
  ),

  # get attribution string
  tar_target(credit, get_credit("CartoDB.PositronNoLabels")),

  # get basemap for inset map
  tar_target(basemap_big, {
    basemap_big <- get_tiles(
      st_bbox(c(xmin = 80, ymin = 20, xmax = 150, ymax = 60), crs = 4326),
      provider = "CartoDB.PositronNoLabels"
    ) %>%
      as.data.frame(xy = TRUE)
    #convert to plotable raster
    if ("red" %in% colnames(basemap_big)) {
      basemap_big <- basemap_big %>%
        mutate(color = rgb(red, green, blue, maxColorValue = 255))
    } else {
      basemap_big <- basemap_big %>%
        mutate(color = rgb(lyr.1, lyr.2, lyr.3, maxColorValue = 255))
    }
    basemap_big
  }),

  # get basemap for main map
  tar_target(basemap, {
    basemap <- get_tiles(locations, provider = "CartoDB.PositronNoLabels") %>%
      as.data.frame(xy = TRUE)
    #convert to plotable raster
    if ("red" %in% colnames(basemap)) {
      basemap <- basemap %>%
        mutate(color = rgb(red, green, blue, maxColorValue = 255)) %>%
        mutate(
          color = if_else(
            color %in% c("#FAFAF8", "#D4DADC"),
            color,
            darken(color, 0.2)
          )
        )
    } else {
      basemap <- basemap %>%
        mutate(color = rgb(lyr.1, lyr.2, lyr.3, maxColorValue = 255)) %>%
        mutate(
          color = if_else(
            color %in% c("#FAFAF8", "#D4DADC"),
            color,
            darken(color, 0.2)
          )
        )
    }
    basemap
  }),

  # create inset map
  tar_target(
    inset_grob,
    ggplotGrob(
      ggplot() +
        geom_raster(data = basemap_big, aes(x, y, fill = color)) +
        scale_fill_identity() +
        geom_sf(data = locations_box) +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) +
        theme_void() +
        theme(
          panel.border = element_rect(
            color = "black",
            fill = NA,
            linewidth = 0.5,
            linetype = "solid"
          )
        )
    )
  ),

  # create main map
  tar_target(
    map,
    ggplot() +
      geom_raster(data = basemap, aes(x, y, fill = color)) +
      scale_fill_identity() +
      # change size here to adjust circle size
      geom_sf(data = locations, aes(color = Species), size = 5) +
      # change colors here to adjust circle colors
      scale_color_manual(name = NULL, values = c("#228833", "#aa3377")) +
      # change breaks here to adjust axis labels
      scale_x_continuous(
        name = NULL,
        expand = c(0, 0),
        breaks = c(95, 105, 115, 125, 135, 145)
      ) +
      scale_y_continuous(
        name = NULL,
        expand = c(0, 0),
        breaks = c(42, 46, 50, 54)
      ) +
      # change x and y, size and label . size here to move attribution and change its size and border width
      geom_label(
        aes(x = 140, y = 41.3, label = credit),
        fill = "white",
        color = "black",
        size = 2,
        label.size = 0.25,
        label.r = unit(0, "lines")
      ) +
      # adjust position of inset map
      annotation_custom(
        grob = inset_grob,
        xmin = 91,
        xmax = 101,
        ymin = 41.5,
        ymax = 46
      ) +
      theme_bw() +
      # adjust legend position (0-1)
      theme(
        legend.position = "inside",
        legend.position.inside = c(0.3, 0.2),
        legend.background = element_rect(
          fill = "white",
          color = "black",
          linewidth = 0.2
        ),
        legend.margin = margin(t = 5, r = 5, b = 5, l = 5)
      )
  )
)
