
day_filter <- function(
  df, 
  day = "" 
) {
  if (day == "Saturday") {
    df <- df %>%
      filter(
        df$birmingham_pharmacy_opening_hours_saturday != "CLOSED"
      )
  }
  else if (day == "Sunday") {
    df <- df %>%
      filter(
        df$birmingham_pharmacy_opening_hours_sunday != "CLOSED"
      )
  } else if (day != "All Days") {
    stop("Unrecognised day filter.")
  }
  
  return(df)
}


get_lsoa_access_sf <- function(
  postcode_df,
  radius_km,
  day
) {
  
  postcode_df <- day_filter(postcode_df, day)

  access_lsoa <- prop_in_radius(postcode_df, radius_km)
  
  sf::st_as_sf(BSol.mapR::LSOA21) %>%
    left_join(
      access_lsoa,
      by = join_by("LSOA21" == "lsoa21_code")
    ) %>%
    filter(Area == "Birmingham") %>%
    sf::st_transform(4326) %>%
    mutate(
      pop_perc = pop_prop*100
    )
}

plot_access_map <- function(
    pharm_access_sf,
    pharm_data,
    day,
    palette = "Blues"
  ) {
  
  pharm_data_pcs <- join_postcode_info(pharm_data)
  
  mypalette <- colorNumeric(
    palette = palette, domain = c(0, 100),
    na.color = "transparent"
  )
  
  perc_labels <- paste0(round(pharm_access_sf$pop_perc,1), "%") %>%
    lapply(htmltools::HTML)
  
  legend_title <- paste0(
    "Estimated Population<br>within 1 mile of a<br>Pharmacy (",
    day, ")")
  
  leaflet(pharm_access_sf) %>%
    addTiles() %>%
    setView(lng = -1.876932, lat = 52.5, zoom = 11) %>%
    addPolygons(
      fillColor = ~ mypalette(pop_perc),
      fillOpacity = 0.8,
      stroke = F,
      label = perc_labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "13px",
        direction = "auto"
      )) %>%
    addLegend("topright", pal = mypalette, values = seq(0, 100, 1),
              title = legend_title,
              labFormat = labelFormat(suffix  = "%"),
              opacity = 1
    ) %>%
   addCircleMarkers(
     data = day_filter(pharm_data_pcs, day), 
     radius = 5,
     stroke = FALSE, 
     fillOpacity = 0.5, 
     popup = ~popup_text,
     color = "yellow"
       )
  
}


get_total_access_perc <- function(
    access_sf
) {
  access_perc = access_sf %>% 
    summarise(
      pop_inside = sum(pop_inside),
      total_pop = sum(lsoa_pop)
    ) %>%
    mutate(
      total_access_perc = round(100 * pop_inside / total_pop,1)
    ) %>%
    pull(total_access_perc)
}

get_open_pharm_count <- function(
    df,
    day
) {
  nrow(day_filter(df, day = day))
}

basic_access_analysis <- function(
    pharm_data,
    radius_km = key_distance_km, 
    day = day
) {
  
  pharm_access_sf <- get_lsoa_access_sf(
    pharm_data, 
    radius_km = key_distance_km, 
    day = day
  )
  
  access_perc <- get_total_access_perc(pharm_access_sf)
  
  open_count <- get_open_pharm_count(pharm_data, day = day)
  
  m <- plot_access_map(
    pharm_access_sf,
    pharm_data,
    day = day
  )
  
  list(
    map = m,
    access_perc = access_perc,
    open_count = open_count
  )
}