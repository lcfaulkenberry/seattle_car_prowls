###########################################################################
# Description: R script to analyze Seattle car prowls
# Author: Leighann Faulkenberry
##########################################################################
# Helper function to determine whether a package is already installed
isPackageInstalled <- function(packageName) {
  return (packageName %in% rownames(installed.packages()))
}

# Helper function that installs a package (if necessary) before requiring
installAndRequire <- function(packageName) {
  if(!isPackageInstalled(packageName)) {
    install.packages(packageName)
  } 
  require(packageName, character.only = TRUE)
}

# Helper function to retrieve crime data by walking pages of Socrata/data.seattle.gov API
get_crime_data <- function(crime_type, limit=5000, logger=NA) {
  offset = 0
  crimes = NA
  while (TRUE) {
    url = URLencode(paste("https://data.seattle.gov/resource/i6vm-ucwh.json",
                          sprintf("?$where=summarized_offense_description = '%s'", crime_type),
                          sprintf("&$limit=%d", limit),
                          sprintf("&$offset=%d", offset), sep = ""))
    loginfo(sprintf("Downloading crime data from %s...", url), logger=logger)
    response_data = fromJSON(url)
    
    # Clean data before adding to result data frame
    response_data$longitude = as.numeric(response_data$location$longitude)
    response_data$latitude = as.numeric(response_data$location$latitude)
    response_data$location = NULL
    response_data$date_reported = as.Date(response_data$date_reported)
    response_data$Month = as.numeric(format(response_data$date_reported, format="%m"))
    response_data$Year = as.numeric(format(response_data$date_reported, format="%Y"))
    
    if (is.data.frame(crimes) && nrow(crimes) > 0) {
      crimes = rbind(crimes, response_data)
    } else {
      crimes = response_data
    }
    if (nrow(response_data) < limit) {
      break;
    }
    offset = offset + limit
  }
  loginfo(sprintf("Downloaded %d rows of crime data.", nrow(crimes)), logger=logger)
  return(crimes)
}

get_neighborhood_shapes <- function(logger=NA) {
  url = "https://data.seattle.gov/download/2mbt-aqqx/application/zip"
  shapes_file = "Neighborhoods.shp"
  wd = getwd()
  uuid = UUIDgenerate()
  
  loginfo(sprintf("Downloading neighborhood shapes zip file from %s...", url), logger=logger)
  download.file(url, destfile = sprintf("%s/%s.zip", wd, uuid))
  
  loginfo(sprintf("Unzipping neighborhood shapes zip file into %s...", sprintf("%s/%s", wd, uuid)), logger=logger)
  unzip(sprintf("%s.zip", uuid), exdir = sprintf("%s/%s", wd, uuid))
  
  loginfo(sprintf("Loading neighborhood shapes file from %s...", sprintf("%s/%s/Neighborhoods/WGS84/%s", wd, uuid, shapes_file)), logger=logger)
  setwd(sprintf("%s/%s/Neighborhoods/WGS84", wd, uuid))
  neighborhoods <- readShapePoly(shapes_file)
  
  setwd(wd)
  
  loginfo(sprintf("Deleting neighborhood shapes zip file at %s...", sprintf("%s/%s.zip", wd, uuid)), logger=logger)
  unlink(sprintf("%s/%s.zip", wd, uuid))
  
  loginfo(sprintf("Deleting neighborhood shapes directory at %s...", sprintf("%s/%s", wd, uuid)), logger=logger)
  unlink(sprintf("%s/%s", wd, uuid))
  
  return(neighborhoods)
}

if (interactive()) {
  installAndRequire('ggmap')
  installAndRequire('ggplot2')
  installAndRequire('jsonlite')
  installAndRequire('logging')
  installAndRequire('maptools')
  installAndRequire('stats')
  installAndRequire('scales')
  installAndRequire('utils')
  installAndRequire('uuid')
  
  # Configure logging
  logger = "lfaulkenberry-seattle-car-prowls"
  basicConfig()
  addHandler(writeToFile, logger=logger, file="lfaulkenberry-seattle-car-prowls.log")
  
  # Download crime data
  crime_data = get_crime_data(crime_type='CAR PROWL', logger=logger)
  
  # Download neighborhood shapes file
  neighborhoods = get_neighborhood_shapes(logger=logger)
  
  # Filter crime data to 2008 - 2014
  crime_data = crime_data[crime_data$year %in% seq(from=2008, to=2014),]
  
  # Determine neighborhood of crimes using neighborhood shapes and crime coordinates
  crimes_spatial_data = crime_data
  coordinates(crimes_spatial_data) <- c("longitude", "latitude")
  proj4string(crimes_spatial_data) <- proj4string(neighborhoods)
  crime_data$neighborhood_object_id <- over(crimes_spatial_data, neighborhoods)$OBJECTID
  crime_data$neighborhood_name <- over(crimes_spatial_data, neighborhoods)$S_HOOD
  remove(crimes_spatial_data)
  
  # Draw a heat map
  loginfo("Generating heat map from data...", logger=logger)
  
  mapImage <- get_map(location = c(lon = -122.335167, lat = 47.608013),
                      color = "color",
                      source = "google",
                      zoom = 12)
  
  ggmap(mapImage) + geom_density2d(data = crime_data, aes(x = longitude, y = latitude), size = 0.3) + 
    stat_density2d(data = crime_data, 
                   aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..), size = 0.01, 
                   bins = 500, geom = "polygon") + scale_fill_gradient(low = "yellow", high = "red") + 
    scale_alpha(range = c(0, 0.8), guide = FALSE)
  
  # Zoom in for a better view of downtown area
  loginfo("Generating heat map from data...", logger=logger)
  mapImage <- get_map(location = c(lon = -122.335167, lat = 47.608013),
                      color = "color",
                      source = "google",
                      zoom = 15)
  
  ggmap(mapImage) + geom_density2d(data = crime_data, aes(x = longitude, y = latitude), size = 0.3) + 
    stat_density2d(data = crime_data, 
                   aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..), size = 0.01, 
                   bins = 100, geom = "polygon") + scale_fill_gradient(low = "yellow", high = "red") + 
    scale_alpha(range = c(0, 0.3), guide = FALSE)
  
  
  # Explore data - show neighborhoods with most car prowls
  crimes_by_neighborhood = as.data.frame(table(crime_data$neighborhood_name))
  names(crimes_by_neighborhood) = c('Neighborhood','Count')
  crimes_by_neighborhood = crimes_by_neighborhood[order(-crimes_by_neighborhood$Count),]
  crimes_by_neighborhood[1:5,] # Show top 5
  
  # Explore data - show months with most car prowls
  crimes_by_month = as.data.frame(table(crime_data$month))
  names(crimes_by_month) = c('Month','Count')
  crimes_by_month$Month = as.numeric(as.character(crimes_by_month$Month))
  crimes_by_month$MonthName = month.abb[crimes_by_month$Month]
  ggplot(data=crimes_by_month, aes(x=Month, y=Count, group=1)) + geom_line(size=1.2) + scale_x_continuous(breaks = 1:12)
  crimes_by_month[order(-crimes_by_month$Count),]
  
  # Explore data - show years with most car prowls
  crimes_by_year = as.data.frame(table(crime_data$year))
  names(crimes_by_year) = c('Year','Count')
  crimes_by_year$Year = as.numeric(as.character(crimes_by_year$Year))
  crimes_by_year[order(-crimes_by_year$Count),]
  
  # Create and plot a linear regression
  model = lm(Count ~ Year, data = crimes_by_year)
  summary(model)
  
  # Predict car prowls in future years
  years_to_predict = data.frame(Year=seq(from=2015, to=2020))
  predictions = as.data.frame(predict(model, years_to_predict))
  predictions$test = years_to_predict$Year
  names(predictions) = c('Count', 'Year')
  
  # Plot
  ggplot(crimes_by_year, aes(x=Year, y=Count)) +
    geom_point(size = 4) +
    geom_smooth(method=lm) + 
    stat_smooth(method=lm, fullrange=TRUE) +
    geom_point(data = predictions, aes(x=Year, y=Count), colour = "red", size = 4) +
    labs(title = "Seattle Car Prowls") + 
    scale_y_continuous("Car Prowl Incident Reports", breaks= pretty_breaks(), labels = comma) +
    scale_x_continuous("Year", breaks= pretty_breaks())
}
