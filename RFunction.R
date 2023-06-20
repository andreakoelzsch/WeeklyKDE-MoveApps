library('move')
library('lubridate')
library('amt')
library('purrr')
library('reshape2')
library('ggplot2')
library('ggforce')
library('leaflet')
library('RColorBrewer')
library('mapview')
library('sf')
library('zip')

## The parameter "data" is reserved for the data object passed on from the previous app

## to display messages to the user in the log file of the App in MoveApps one can use the function from the logger.R file: 
# logger.fatal(), logger.error(), logger.warn(), logger.info(), logger.debug(), logger.trace()

rFunction = function(data) {

  # MAKING A MOVEMENT TRACK  
  #minimum of 1 minute sampling in the data
  data <- data[!duplicated(paste0(lubridate::round_date(timestamps(data), "1 mins"), trackId(data))),]
 
  #convert to dataframe
  data_df <- as(data, "data.frame")
  
  #make movement track
  data_track <- make_track(data_df,.x = location.long,
                           .y = location.lat,
                           .t = timestamps,
                           id = individual.local.identifier,
                           crs = 4326)
  
  #output 1: export as csv
  write.csv(data_track, file = appArtifactPath("data_track.csv"), row.names = FALSE)
  
  #prepare dataframe of tracks per id
  id_track_list <- data_track %>% 
    nest(info = -c(id)) 
  
  #plot as html map
  col <- brewer.pal(8, "Reds") 
  
  plot_fun <- function(x){
    pal <- colorNumeric(
      palette = col,
      domain = as.numeric(as.factor(x$t_))) #create palette
    
    inspect(x, cluster = FALSE) %>%
      addPolylines(~x_, ~y_, 
                   weight = 5)%>%
      addCircleMarkers(
        ~x_, ~y_,
        radius = 5,
        weight = 0.1,
        popup = ~ paste0(t_),
        color = ~pal(as.numeric(as.factor(x$t_))))
  }
  
  plots <- lapply(id_track_list$info, plot_fun)
  
  #output 2: individual html map plots
  # Create a temporary directory
  dir.create(targetDirHtmlFiles <- tempdir())
  
  # Loop through plots and export each as an HTML file in the temporary directory
  for (i in seq_along(plots)) {
   mapshot(plots[[i]], url = file.path(targetDirHtmlFiles, paste0("map_", id_track_list$id[[i]], ".html")))
  }
  #plots in temp directory will be exported into map_html_files.zip folder in output

  # MAKING WEEKLY TRACK SEGMENTS  
  #get weekly intervals 
  data_track <- data_track %>% 
    arrange(id,t_) %>%
    mutate(date = as.Date(data_track$t_, format = "%Y-%m-%d"),
           week = cut(date, breaks=seq.Date(min(date), 
                                            max(date)+7,
                                            by = 7)))
  
  #nest by individual and week
  track_list <- data_track %>% 
    nest(info = -c(id, week)) 
  
  #calculate some movement metrics
  track_list <- track_list %>%
    mutate(
      straightness = as.numeric(map(track_list$info, possibly(straightness, otherwise = NA))), #straightness
      cum_dist = as.numeric(map(track_list$info, possibly(cum_dist, otherwise = NA))), #cumulative distance
      tot_dist = as.numeric(map(track_list$info, possibly(tot_dist,otherwise = NA))), #total distance
      msd = as.numeric(map(track_list$info, possibly(msd,otherwise = NA))), #max squared displacement
      intensity_use = as.numeric(map(track_list$info, possibly(intensity_use,otherwise = NA))), #total movement by area of movement
      sinuosity = as.numeric(map(track_list$info, possibly(sinuosity,otherwise = NA))), #sinuosity
      tac = as.numeric(map(track_list$info, possibly(tac,otherwise = NA))), #turning angle correlation
    ) 
  
  #output 3: export metrics as csv
  track_list_df <- track_list %>% select(-info)
  write.csv(track_list_df, file = appArtifactPath("track_list_df.csv"), row.names = FALSE)
  
  #melt dataframe to plot with all metrics
  track_list_melt <- melt(track_list, na.rm = TRUE, id = c("id", "week","info"))
  
  #output 4: export metric plots into PDF
  metric_plts <- track_list_melt %>% 
    split(.$id) %>% 
    map(~ggplot(data = .x, 
                mapping = aes(x = week,  y = log1p(value), color = variable, group = variable)) + 
          geom_point()+
          geom_path()+
          stat_smooth(geom='line', alpha=0.75, se=FALSE)+
          facet_wrap_paginate(~id, scales = "free", ncol = 1, nrow = 1)+
          theme_minimal()+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)),
        legend.position = "none")
  
  pdf(appArtifactPath("metric_plots.pdf"),onefile = TRUE)
  walk(metric_plts, print)
  dev.off()
  
  # KDE WEEKLY HR ESTIMATION  
  #hr estimation
  hr <- list()
 
  hr_list <- track_list[,1:3] %>% #get rid of unneeded cols
    mutate(
      row = sapply(track_list$info, nrow)
    ) %>%
    filter(row > 10)
  
  hr <- hr_list %>%
    mutate( 
      hr_kde = (map(hr_list$info, ~hr_kde(., levels = c(0.50, 0.95)))), #probabilistic
    )   
  
  #hr plots per individual and week
  hr_check <- hr %>%
    mutate(hr = map(hr_kde, possibly(hr_area, otherwise = NA))) %>%
    unnest(hr) %>% na.omit() %>% filter(level == 0.95)
  
  #output 5: export KDE plots per ID and week
  pdf(appArtifactPath(paste("hr_plot.pdf")))
  for(i in 1:nrow(hr_check)){
    plot(hr_ud(hr_check$hr_kde[[i]]))
    plot(hr_check$hr_kde[[i]],  add.relocations = TRUE, add = TRUE)
    title(main = paste(hr_check$id[i]," ", hr_check$week[i]))
  }
  dev.off()
  
  #hr changes through time
  hr_all <- hr %>%
    mutate(hr = map(hr_kde, possibly(hr_area, otherwise = NA))) %>%
    unnest(hr) %>% na.omit()
  
  #plot as pdf
  hr_time_plts <- hr_all %>% 
    mutate(id = as.factor(id)) %>%
    split(.$id) %>% 
    map(~ggplot(data = .x, 
                mapping = aes(x = week, y = log1p(area), color = as.factor(level), group = as.factor(level))) + 
          geom_point()+
          geom_path()+
          facet_wrap_paginate(~id, scales = "free", ncol = 1, nrow = 1)+
          theme_minimal()+
          labs(color = "KDE") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))
  
  #output 6: export KDE by week
  pdf(appArtifactPath("hr_time_plots.pdf"),onefile = TRUE)
  walk(hr_time_plts, print)
  dev.off()
  
  #output 7: export hr data as csv
  hr_df <- hr_all %>% select(-c(info, hr_kde, row))
  write.csv(hr_df, file = appArtifactPath("hr_df.csv"), row.names = FALSE)
  
  # WHAT TIME ARE BIRDS IN CORE AREA?
  #extract isopleths (polygons)
  hr_values <- hr_all %>% 
    mutate(isopleth = map(hr_kde, possibly(hr_isopleths, otherwise = "NA"))) %>%
    filter(isopleth != "NA")
  
  isopleths <- unique(do.call(rbind, hr_values$isopleth))
  class(isopleths)
  isopleths$id <- hr_values$id
  isopleths$week <- hr_values$week
  isopleths$week_num <- as.numeric(as.factor(as.character(isopleths$week)))
  
  last_week <- isopleths %>% 
    group_by(id,level) %>% 
    arrange(week)%>% 
    dplyr::slice_tail()
  
  #select the last week of data points 
  last_week_pts_list <- hr_values%>% 
    group_by(id,level) %>% 
    arrange(week)%>% 
    dplyr::slice_tail()
  
  last_week_pts_list$source <- rownames(last_week_pts_list)
  last_week_pts <- dplyr::bind_rows(list(last_week_pts_list$info), .id = 'source')
  last_week_pts_final <- merge(last_week_pts_list, last_week_pts, by = "source")
  
  #output 8: export last week points as csv
  last_week_pts_final_df <- last_week_pts_final %>% select(-c(info, hr_kde, isopleth))
  write.csv(last_week_pts_final_df, file = appArtifactPath("last_week_pts_df.csv"),row.names = FALSE)
  
  #plot last week .50 and .95 and points as html map
  col <- brewer.pal(8, "Spectral") 
  pal <- colorNumeric(
    palette = col,
    domain = as.numeric(as.factor(last_week_pts_final$id))) #create palette
  
  last_week_plot <- leaflet() %>%
    addTiles() %>%
    addPolygons(data = last_week, weight = 1, 
                fillColor = "darkgrey",
                popup = ~ paste0(id," ", week)) %>%
    addCircleMarkers(data = last_week_pts_final,
                     lng = ~x_, lat = ~y_, 
                     radius = 3,
                     popup = ~paste0(id," ",t_),
                     color = ~pal(as.numeric(as.factor(last_week_pts_final$id))))
  
  #output 9: export last week plot
  #also exporting these plots into the temporary directory
  mapshot(last_week_plot, url = file.path(targetDirHtmlFiles, paste0("map", "_last_week_plot.html")))
  
  # zip HTML files from temporary directory in output
  zip_file <- appArtifactPath(paste0("map_html_files.zip"))
  zip::zip(zip_file, 
      files = list.files(targetDirHtmlFiles, full.names = TRUE,
                                   pattern="^map_"),
      mode = "cherry-pick")
  #map_html_files.zip contains individual + last week maps
  
  #intersecting last week core area with datapoints for respective individuals
  core_area <- last_week %>% filter(level == 0.50)
  
  last_week_pts_df <- as.data.frame(last_week_pts_final)
  last_week_pts_df<- last_week_pts_df%>% select(-info)
  
  coordinates(last_week_pts_df) <-  c("x_", "y_")
  proj4string(last_week_pts_df) <- CRS("+proj=longlat +datum=WGS84")
  pts_proj <- spTransform(last_week_pts_df, CRS("EPSG:4326"))
  class(pts_proj)
  
  pts_sf <- sf::st_as_sf(x = pts_proj, 
                         coords = c("x_", "y_"),
                         crs = "EPSG:4326")
  
  intersect_dat <- pts_sf %>% mutate(
    intersection = as.character(st_intersects(geometry, core_area$geometry)),
    intersect = as.numeric(intersection),
    location = dplyr::if_else(is.na(intersect), "0", paste0("1"))) 
  
  intersect_df <- as.data.frame(intersect_dat)
  
  time_in_core <- intersect_df %>% 
    filter(location == 1) %>%
    mutate(time = as.numeric(format(t_, "%H")))
  
  #output 10: export points in core as csv
  time_in_core_df <- time_in_core %>% select(-c(row, hr_kde, isopleth))
  write.csv(time_in_core_df, file = appArtifactPath("time_in_core.csv"), row.names = FALSE)
  
  #plot weekly count of fixes in core by time as pdf
  wk_time_core_plts <- time_in_core %>% 
    mutate(id = as.factor(id)) %>%
    split(.$id) %>% 
    map(~ggplot(data = .x, 
                mapping = aes(x = time)) + 
          geom_histogram()+
          xlim(0,23)+
          facet_wrap_paginate(~id, scales = "free", ncol = 1, nrow = 1)+
          theme_minimal())
  
  #output 11: export weekly times in core
  pdf(appArtifactPath("wk_time_core_plots.pdf"),onefile = TRUE)
  walk(wk_time_core_plts, print)
  dev.off()
  
  #plot daily count of fixes in core by time as pdf
  day_time_core_plts <- time_in_core %>% 
    mutate(id = as.factor(id)) %>%
    split(.$date) %>% 
    map(~ggplot(data = .x, 
                mapping = aes(x = time)) + 
          geom_histogram()+
          xlim(0,23)+
          facet_wrap_paginate(~id+date, scales = "free", ncol = 1, nrow = 1)+
          theme_minimal())
  
  #output 12: export daily times in core
  pdf(appArtifactPath("day_time_core_plots.pdf"),onefile = TRUE)
  walk(day_time_core_plts, print)
  dev.off()
  
  return(data)
  }
