## UD
load("UDS_month_wolves.RData") # load of monthly UDs

hrs_8 <- lapply(UDS_8, function(d) as.sf(d, level.UD = 0.95)) # selection of the wolf

names(hrs_8) <- paste("period",1:length(hrs_8))

hrs_8 <- lapply(names(hrs_8), function(period){
  hr <- hrs_8[[period]]
  hr$period <- period
  hr$type <- strsplit(hr$name, split=" ", fixed=T) %>% sapply(function(item) item[3])
  hr
}) %>% 
  bind_rows() 

hrs_8 <- hrs_8 %>% filter(type=="est")
hrs_8$month <- names(UDS_8)
month_y <- c("October - 2022", "November - 2022", "December - 2022",
             "January - 2023", "February - 2023", "March - 2023", "April - 2023",
             "May - 2023", "June - 2023", "July - 2023")
hrs_8$month_y <- month_y


places <- readOGR("ne_10m_populated_places.shp")
shp <- readOGR("Places_krovak.shp")
hill_47016 <- stack("Austria.tif") # load and prepare raster
hill_47016 <- setMinMax(hill_30776)
rast_df  <- as.data.frame(hill_30776, xy = TRUE) %>% drop_na() 
hrs_8_krov <- st_transform(hrs_8, crs(hill_47016))
hrs_8_krov$month_y <- month_y




hrs_8_krov <- st_transform(hrs_8, crs(places))
hrs_8_krov<- tibble::rowid_to_column(hrs_8_krov, "index")
hrs_8_krov$index <- as.numeric(hrs_8_krov$index)

shape <- raster::crop(shp, hill_30776)
shape<-as.sf(shape)


# animation of monthly HRs
akde <- ggplot(hrs_8_krov) +  
  geom_sf(data = test, fill = "grey", color = "white", show.legend = F) +
  geom_sf(data=places, show.legend = F) +
  geom_raster(data=rast_df, aes(x=x,y=y, fill = layer), 
              alpha = 0.2, show.legend = F) + # add raster  
  scale_fill_gradientn(colors = pal_greys, na.value = NA) +
  geom_sf_text(data = test[1,], aes(label = SOVEREIGNT), family = "sans",
               vjust = -5, size = 10) + # Manage layer descriptions
  geom_sf_text(data = test[2,], aes(label = SOVEREIGNT), family = "sans", hjust = 0, vjust = 4,
               size =10) + 
  geom_sf_text(data = test[3,], aes(label = SOVEREIGNT), family = "sans", vjust = -2, hjust = 0.8,
               size = 10) +
  geom_sf_label(data = places ,aes(label = NAME), label.padding = unit(1, "mm"), vjust = -0.5,
                size = 7) +
  geom_sf(alpha = 0.8) +
  theme_minimal() + 
  transition_states(as.factor(index), transition_length = 3, state_length = 3) +
  labs(
    title = "Study area E - ID 47016 : {hrs_8_krov %>% filter(index == closest_state) %>% pull(month_y)}",
  ) +
  theme(plot.title = element_text(size = 30, face = "bold")) 

gganimate::animate(akde, height = 800, width =800) # creating an animation with the required width and height
anim_save("47016_AKDE.gif") # export animation in GIF format
