library(dplyr)
library(lubridate)
library(sf)
library("psych")
library(kableExtra)

# Reading the data

setwd("C:/Users/marsi/Desktop/Szkoła/Wiedeń I/LBS")

data = read.csv('full_intersection_16.csv',sep = ";", dec = ".")


#Date filtering

data_processed = filter(data, tracking_status == "TRACKING")

data_processed$date = as.POSIXct(data_processed$timestamp / 1000, origin = "1970-01-01")

#Some clean-up

data_processed = select(data_processed, -c(timestamp, tracking_status))


#Further filtering, misc is out

data_processed = filter(data_processed, object_type != "MISC")

#Changing velocities to km/h
data_processed$v = data_processed$v*3.6

#Drawing trajectory lines
data_georeferenced = st_as_sf(data_processed, coords = c("lon", "lat"), 
                 crs = 4326)


##Area histograms for groups
hist(filter(data_georeferenced, object_type == 'PEDESTRIAN')$width, 
     main = 'Distribution of width for pedestrians',
     xlab = 'Width in metres')

hist(filter(data_georeferenced, object_type == 'CAR')$width, 
     main = 'Distribution of width for cars',
     xlab = 'Width in metres')

hist(filter(data_georeferenced, object_type == 'CYCLIST')$width, 
     main = 'Distribution of width for two-wheelers',
     xlab = 'Width in metres')


hist(filter(data_georeferenced, object_type == 'PEDESTRIAN')$length, 
     main = 'Distribution of Length for pedestrians',
     xlab = 'Length in metres')

hist(filter(data_georeferenced, object_type == 'CAR')$length, 
     main = 'Distribution of Length for cars',
     xlab = 'Length in sq metres')

hist(filter(data_georeferenced, object_type == 'CYCLIST')$length, 
     main = 'Distribution of Length for two-wheelers',
     xlab = 'Length in sq metres')


## Harmonic mean has been used for avg velocities
data_georeferenced_lines = group_by(data_georeferenced, object_id)
data_georeferenced_lines = summarise(data_georeferenced_lines, 
                                     st_cast(st_combine(geometry),"MULTILINESTRING"),
                                     v_avg = harmonic.mean(v, zero = FALSE),
                                     v_median = median(v),
                                     v_min = min(v),
                                     v_max = max(v),
                                     v_q1 = quantile(v, 0.25),
                                     v_q95 = quantile(v, 0.95),
                                     object_area = quantile(height,0.95)*quantile(width,0.95),
                                     count_cars = length(which(object_type=='CAR')),
                                     count_cyclist = length(which(object_type=='CYCLIST')),
                                     count_pedestrian = length(which(object_type=='PEDESTRIAN'))
)




##Calculating line lengths - this will be one of the filters

data_georeferenced_lines$line_len = st_length(data_georeferenced_lines[2])



#Filtering out all objects with trajectories shorter than 20m

##Histogram of trip lengths

hist(data_georeferenced_lines$line_len, main = 'Histogram of trajectory lines length',
     xlab = 'Trajectory length')


##Quantiles calculation
IQR_len = as.numeric(IQR(data_georeferenced_lines$line_len))
len_Q1 = as.numeric(quantile(data_georeferenced_lines$line_len, 0.25))
len_Q3 = as.numeric(quantile(data_georeferenced_lines$line_len, 0.75))


##Remove outliers using IQR. It doesn't make sense for the lower quantile.
##The threshold has been chosen arbitrarly, based on QGIS visualization - 40m

data_georeferenced_lines = filter(data_georeferenced_lines, 
                                  as.numeric(line_len) > 40)

##Apply this filter to the main dataset
data_georeferenced = filter(data_georeferenced, 
                            object_id %in% data_georeferenced_lines$object_id)




#Absolute distances between points

##the first point of trajectory
min = data_georeferenced
min %>%
  group_by(object_id) %>%
  arrange((date)) %>%
  filter(row_number()==1) -> min


min = rename(min, start_point = geometry)
min = min[order(min$object_id),]


##the last point of trajectory
max = data_georeferenced
max %>%
  group_by(object_id) %>%
  arrange((date)) %>%
  filter(row_number()==n()) -> max

max = rename(max, end_point = geometry)
max = select(max, c(1,9))
max = max[order(max$object_id),]


## calculating distances, these will serve as a data filter later

min$absolute_dist = st_distance(min,max, by_element = TRUE)

hist(min$absolute_dist, main = 'Histogram of point to point distances',
     xlab = 'Distance')

##Remove outliers using chosen threshold. The value of the first quartile - 33
##The threshold has been chosen arbitrarly, based on QGIS visualization - 30m

min = filter(min, as.numeric(absolute_dist) > 30)

###Apply the filter to the end point data frame
max = filter(max, object_id %in% min$object_id)

###Apply the filter to the point data frame
data_georeferenced  = filter(data_georeferenced, object_id %in% min$object_id)

###Apply the filter to the line data frame
data_georeferenced_lines  = filter(data_georeferenced_lines, object_id %in% min$object_id)


#Detecting the anomalies of changing the object type
data_ch_objects = group_by(data_georeferenced, object_id)
data_ch_objects = summarise(data_ch_objects,
                            (changes=paste(unique(na.omit(object_type)), collapse = ', ')))
colnames(data_ch_objects)[2] <- "type_changes"


##counting groups
count_changes = count(data_ch_objects, type_changes)


##joining groups to lines
data_lines_groups = cbind(data_georeferenced_lines, 
                          select(as.data.frame(data_ch_objects),type_changes))



##Counting avg velocities in groups
data_lines_groups_v = group_by(as.data.frame(data_lines_groups), type_changes)
data_lines_groups_v = summarise(data_lines_groups_v,
                                     v_avg_group = harmonic.mean(v_avg, zero = FALSE),
                                     v_median_group = median(v_median),
                                      v_min_group = min(v_min),
                                      v_max_group = max(v_max),
                                      v_q1_group = quantile(v_q1, 0.25),
                                      v_q95_group = quantile(v_q95, 0.95),
                                      object_avg_area_group = mean(object_area),
                                      object_median_area_group = median(object_area),
                                      object_min_area_group = min(object_area),
                                      object_max_area_group = max(object_area),
                                      object_q1_area_group =  quantile(object_area, 0.25),
                                      object_q95_area_group =  quantile(object_area, 0.95)
                                      )


##Velocity histograms for groups
hist(filter(data_lines_groups, type_changes == 'PEDESTRIAN')$v_max, 
     main = 'Distribution of maximum velocities for pedestrians',
     xlab = 'Velocity km/h')

hist(filter(data_lines_groups, type_changes == 'CAR')$v_max, 
     main = 'Distribution of maximum velocities for cars',
     xlab = 'Velocity km/h')

hist(filter(data_lines_groups, type_changes == 'CYCLIST')$v_max, 
     main = 'Distribution of maximum velocities for two-wheelers',
     xlab = 'Velocity km/h')



##Area histograms for groups
hist(filter(data_lines_groups, type_changes == 'PEDESTRIAN')$object_area, 
     main = 'Distribution of object areas for pedestrians',
     xlab = 'Area in sq metres')

hist(filter(data_lines_groups, type_changes == 'CAR')$object_area, 
     main = 'Distribution of object areas for cars',
     xlab = 'Area in sq metres',
     xlim = c(0, 25),
     breaks = 21)

hist(filter(data_lines_groups, type_changes == 'CYCLIST')$object_area, 
     main = 'Distribution of object areas for two-wheelers',
     xlab = 'Area in sq metres')


##tables with given statistics
select(data_lines_groups_v, c(1:7)) %>% 
  kbl(caption = 'Velocities of object types in km/h',col.names = c('Object Type', 'Average', 'Median','Minimum','Maximum', '25th Quantile', '95th Quantile')) %>% 
  kable_minimal

select(data_lines_groups_v, c(1, 8:13)) %>% 
  kbl(caption = 'Areas of object types in sq metres',col.names = c('Object Type', 'Average', 'Median','Minimum','Maximum', '25th Quantile', '95th Quantile')) %>% 
  kable_minimal


# Estimating the final object types. Majority of points from one category change the category of entire object
data_lines_final = data_lines_groups
data_lines_final$object_type[data_lines_final$count_cars > data_lines_final$count_cyclist & 
                               data_lines_final$count_cars > 
                               data_lines_final$count_pedestrian] <- 'CAR'

data_lines_final$object_type[data_lines_final$count_cyclist > data_lines_final$count_cars & 
                               data_lines_final$count_cyclist > 
                               data_lines_final$count_pedestrian] <- 'CYCLIST'

data_lines_final$object_type[data_lines_final$count_pedestrian > data_lines_final$count_cars & 
                               data_lines_final$count_pedestrian > 
                               data_lines_final$count_cyclist] <- 'PEDESTRIAN'


st_write()
##Velocity histograms for groups
hist(filter(data_lines_final, type_changes == 'PEDESTRIAN')$v_max, 
     main = 'Distribution of maximum velocities for pedestrians',
     xlab = 'Velocity km/h')

hist(filter(data_lines_final, type_changes == 'CAR')$v_max, 
     main = 'Distribution of maximum velocities for cars',
     xlab = 'Velocity km/h')

hist(filter(data_lines_final, type_changes == 'CYCLIST')$v_max, 
     main = 'Distribution of maximum velocities for two-wheelers',
     xlab = 'Velocity km/h')


## Exporting the final file for exploration in QGIS
data_lines_final_export = select(data_lines_final, c(1:8, 12,15,14))
write_sf(data_lines_final_export, 'grouped_trajectories.shp')

write_sf(data_lines_groups, 'ungroupped_trajectories.shp')

write_sf(data_georeferenced, 'ungroupped_points.shp')

write_sf(data_georeferenced, 'ungroupped_points.shp')

data_georeferenced_groupped = data_processed

data_georeferenced_groupped = select(filter(data_georeferenced_groupped, 
                            object_id %in% data_georeferenced_lines$object_id),c(-7))

data_georeferenced_groupped = inner_join(data_georeferenced_groupped, data_lines_final_export, by = 'object_id')
data_georeferenced_groupped = select(data_georeferenced_groupped, c(1:9, 18))
data_georeferenced_groupped = st_as_sf(data_georeferenced_groupped, coords = c("lon", "lat"), 
                              crs = 4326)
st_write(data_georeferenced_groupped, 'points_groupped.shp')



#Final filter for the data

final_filter = st_read('Threshold 60/ungroupped_trajectories_60.shp')


#Final end and starting points

min_final  = filter(min, object_id %in% final_filter$objct_d)
min = min[order(min$object_id),]

max_final  = filter(max, object_id %in% final_filter$objct_d)
max = max[order(max$object_id),]

#Polygons for the end-start
start_end = st_read('endings.shp')

min_join = st_join(min_final, start_end, join = st_intersects)
min_join = select(min_join,c(1,7,12))
colnames(min_join)[3] <- "start_point"

max_join = st_join(max_final, start_end, join = st_intersects)
max_join = select(max_join,c(1,4))
colnames(max_join)[2] <- "end_point"

destinations_join = cbind(min_join,max_join)
destinations_join$trips = paste(destinations_join$start_point, '-' ,destinations_join$end_point)


#Travel table

travels = count(destinations_join, trips)
travels = travels[order(travels$n, decreasing = TRUE),]

select(travels, c(1,2)) %>% 
  kbl(caption = 'Trips from - to. All object types',col.names = c('Start-End Point', 'Trip Count', 'Geom')) %>% 
  kable_minimal


#Travel-by-category table

travels_cat = group_by(destinations_join, object_type, trips)
travels_cat = summarize(travels_cat,
                        n = length(trips))

travels_car = filter(travels_cat, object_type == 'CAR')
travels_car = travels_car[order(travels_car$n, decreasing = TRUE),]

select(travels_car, c(1,2,3)) %>% 
  kbl(caption = 'Trips from - to. Cars only',col.names = c('CAT','Start-End Point', 'Trip Count', 'Geom')) %>% 
  kable_minimal

travels_cyc = filter(travels_cat, object_type == 'CYCLIST')
travels_cyc = travels_cyc[order(travels_cyc$n, decreasing = TRUE),]

select(travels_cyc, c(1,2,3)) %>% 
  kbl(caption = 'Trips from - to. Two-Wheelers only',col.names = c('CAT','Start-End Point', 'Trip Count', 'Geom')) %>% 
  kable_minimal

travels_ped = filter(travels_cat, object_type == 'PEDESTRIAN')
travels_ped = travels_ped[order(travels_ped$n, decreasing = TRUE),]

select(travels_ped, c(1,2,3)) %>% 
  kbl(caption = 'Trips from - to. Pedestrians only',col.names = c('CAT','Start-End Point', 'Trip Count', 'Geom')) %>% 
  kable_minimal
