
day_filter <- function(
  df, 
  day_filter = "" 
) {
  if (day_filter == "Saturday") {
    df <- df %>%
      filter(
        df$birmingham_pharmacy_opening_hours_saturday != "CLOSED"
      )
  }
  else if (day_filter == "Sunday") {
    df <- df %>%
      filter(
        df$birmingham_pharmacy_opening_hours_sunday != "CLOSED"
      )
  } else if (day_filter != "") {
    stop("Unrecognised day filter.")
  }
  
  return(df)
}


get_lsoa_access_sf <- function(
  lsoa_data,
  radius_km,
  day_filter = ""
) {
  
  pharm_data <- day_filter(lsoa_data, day_filter)
  
  access_lsoa <- prop_in_radius(pharm_data, radius_km)
  
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
    pharm_data_pcs,
    legend_title,
    day_filter = "",
    palette = "Blues"
  ) {
  
  mypalette <- colorNumeric(
    palette = palette, domain = pharm_access_1km_sf$pop_perc,
    na.color = "transparent"
  )
  
  perc_labels <- paste0(round(pharm_access_sf$pop_perc,1), "%") %>%
    lapply(htmltools::HTML)
  
  leaflet(pharm_access_sf) %>%
    addTiles() %>%
    setView(lng = -1.876932, lat = 52.4777, zoom = 11) %>%
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
    addLegend("topright", pal = mypalette, values = ~pop_perc,
              title = "Estimated Population within<br>1km of a Pharmacy",
              labFormat = labelFormat(suffix  = "%"),
              opacity = 1
    ) 
  
  #%>%
  # addCircleMarkers(
  #   data = day_filter(pharm_data_pcs, day_filter), 
  #   radius = 5,
  #   stroke = FALSE, 
  #   fillOpacity = 0.5, 
  #   popup = ~popup_text,
  #   color = "yellow"
  #     )
  
}