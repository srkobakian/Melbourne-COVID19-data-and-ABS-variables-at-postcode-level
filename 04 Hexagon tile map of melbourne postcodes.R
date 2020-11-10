library(sugarbag)
library(tidyverse)

# Make a hexagon tile map for the postal areas
postcode_hexagons <- create_hexmap(mapdata, "Postcode", hex_size = 0.04, buffer_dist = 2.5, hex_filter = 15)

ggplot(postcode_hexagons) + geom_point(aes(x=hex_long, y = hex_lat))

hexagon_polygons <- fortify_hexagon(postcode_hexagons, sf_id = "Postcode", hex_size = 0.04)

# Turn hexagon polygons to sfc polygons
hexagons_sfc <- hexagon_polygons %>% 
  sf::st_as_sf(coords = c("long", "lat"), crs=4326, agr = "constant") %>% 
  group_by(Postcode) %>% 
  summarise(do_union=FALSE) %>%
  sf::st_cast("POLYGON") %>% left_join(st_drop_geometry(dat_final))

mapdata_polygons <- fortify_sfc(mapdata)

# load data for colouring map areas
load("Melbourne.spatial.COVID19.RDS")

# plotly vis

vis <- hexagons_sfc %>% select(Postcode, Suburb, Population, Cases.per.100K, `Confirmed cases (ever)`, Include )

vis$Cases.per.100K <- ifelse(vis$Include == "No", 0, vis$Cases.per.100K)

g <- list(showlegend = FALSE,
          showframe = FALSE,
          showcoastlines = FALSE,
          projection = list(type = 'Mercator')
)


geospatial.plot <- vis %>%
  plot_geo(split = ~Postcode, showlegend = FALSE, hoverinfo = "text", color = ~Cases.per.100K, 
           colors = viridis_pal(option = "B")(30),
           text = ~paste("Postcode:", Postcode, 
                         "<br>","Suburb:", Suburb,
                         "<br>","Cases per 100K:", Cases.per.100K,
                         "<br>","All cases 6 Aug:", `Confirmed cases (ever)`
           )) %>%
  add_sf() %>%
  layout(geo = g)

colorbar(geospatial.plot, len = 0.4, title = "Cases per 100K 6 Aug")  


geospatial.plot
