library(ggplot2)
library(dplyr)
library(leaflet)
library(bcctheme)

day_filter <- function(
  df, 
  day = "" 
) {
  if (day == "Saturday") {
    df <- df %>%
      filter(
        pharmacy_opening_hours_saturday != "CLOSED"
      )
  }
  else if (day == "Sunday") {
    df <- df %>%
      filter(
        pharmacy_opening_hours_sunday != "CLOSED"
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
    palette = bcc_pal(palette = "purple", reverse = TRUE)(10)
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
    addTiles(
      urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}",
      attribution = "Tiles &copy; Esri"
    ) %>%
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
     fillOpacity = 0.6, 
     popup = ~popup_text,
     color = bcc_cols("yellow")[[1]]
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
    day,
    area = "All"
) {
  if (area %in% unique(df$health_and_wellbeing_board)) {
    df <- df %>%
      filter(
        health_and_wellbeing_board == area
      )
  } else if(area != "All") {
    stop("Unrecognised area given to get_open_pharm_count()")
  }

  nrow(day_filter(df, day = day))
}

basic_access_analysis <- function(
    pharm_data,
    radius_km, 
    day,
    radii_km_list,
    eth_data
) {
  
  pharm_access_sf <- get_lsoa_access_sf(
    pharm_data, 
    radius_km = key_distance_km, 
    day = day
  )
  
  access_perc <- get_total_access_perc(pharm_access_sf)
  
  open_count <- get_open_pharm_count(
    pharm_data, 
    day = day, 
    area = "Birmingham"
    )
  
  m <- plot_access_map(
    pharm_access_sf,
    pharm_data,
    day = day
  )
  
  var_dist_df <- variable_distance_access(pharm_data, day, radii_km_list)
  
  p <- plot_variable_dist_access(var_dist_df, day)
  
  eth_access <- calc_eth_access(
    pharm_access_sf,
    eth_data
  )
  
  eth_plot <- plot_eth_access(
    eth_access, 
    day
    )
  
  list(
    map = m,
    access_perc = access_perc,
    open_count = open_count,
    var_dist = var_dist_df,
    var_dist_plot = p,
    eth_access = eth_access,
    eth_plot = eth_plot
  )
}

variable_distance_access <- function(
    pharm_data,
    day,
    radii_km_list
  ) {
    
  access_percs <- c()
  
  for (radius_i in radii_km_list) {
    sf_i <- get_lsoa_access_sf(
      pharm_data,
      radius_i,
      day
    )
    
    access_percs <- c(access_percs, get_total_access_perc(sf_i))
  }
  
  data <- data.frame(
    radii_km = radii_km_list,
    access_perc = access_percs 
  ) %>%
    mutate(
      radii_miles = radii_km / 1.609344
    ) %>%
    select(
      radii_km, radii_miles, access_perc
    )
  
  return(data)
}

plot_variable_dist_access <- function(
    access_perc_array,
    day
) {
  ggplot(
    access_perc_array %>%
      mutate(
        radii_miles = as.factor(radii_miles),
        radii_miles = paste0("<", radii_miles)
      ), 
    aes(
      x = radii_miles,
      y = access_perc
    )
    ) + 
    geom_col(fill = bcc_cols("purple")) +
    theme_bcc(base_size = 12)  +
    scale_y_continuous(
      expand = c(0,0),
      limits = c(0, 100),
      labels = scales::label_number(suffix = "%")
      ) + 
    labs(
      y = paste0("Estimated Population\nProportion (",day, ")"),
      x = "Distance from Pharmacy (miles)"
    )
}

calc_eth_access <- function(
  access_props,
  eth_data
  ) {
  access_props %>%
    left_join(
      eth_data,
      by = join_by("LSOA21" == "lsoa21_code"),
      relationship = "one-to-many"
    ) %>%
    group_by(ethnicity) %>%
    summarise(
      n = sum(bsol_registrants * pop_prop),
      N = sum(bsol_registrants)
    ) %>%
    mutate(
      p = n / N,
      access_perc = round(100 * p, 1),
      Z = qnorm(0.975),
      ci_95_lower = round(100 * (p + Z^2/(2*N) - Z * sqrt((p*(1-p)/N) + Z^2/(4*N^2))) / (1 + Z^2/N),1),
      ci_95_upper =  round(100 * (p + Z^2/(2*N) + Z * sqrt((p*(1-p)/N) + Z^2/(4*N^2))) / (1 + Z^2/N),1)
    )
}

plot_eth_access <- function(
    eth_data,
    day
) {
  ggplot(
    eth_data, 
    aes(
      y = ethnicity,
      x = access_perc
    )
  ) + 
    geom_col(fill = bcc_cols("purple")) +
    theme_bcc(base_size = 12) +
    scale_x_continuous(
      expand = c(0,0),
      limits = c(0, 100),
      labels = scales::label_number(suffix = "%")
    ) + 
    geom_errorbar(
      aes(xmin = ci_95_lower, xmax = ci_95_upper),
      width = 0.5
      ) +
    labs(
      x = paste0("Estimated Population Proportion within\n1 mile of a Pharmacy (",day, ")"),
      y = ""
    )
}