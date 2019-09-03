##### 
# Get and Validate GTFS data
# 
# This Script downloads and validates the service end dates for all known WA GTFS feeds.  
#####

setwd("Location where you want files saved.")

#####
# URLs and Save Names
#####
GtfsUrls <- c("http://ridethevalley.org/gtfs-feed.zip", 
               "https://www.bft.org/gtfs/google_transit.zip",
               "http://mjcaction.com/MJC_GTFS_Public/clallam_google_transit.zip",
               "http://developer.onebusaway.org/tmp/sound/gtfs/modified/29_gtfs.zip",
               "http://www.c-tran.com/images/Google/GoogleTransitUpload.zip",
               "http://developer.onebusaway.org/tmp/sound/gtfs/modified/97_gtfs.zip",
               "http://www.mjcaction.com/MJC_GTFS_Public/graysharbor_google_transit.zip",
               "http://developer.onebusaway.org/tmp/sound/gtfs/modified/19_gtfs.zip",
               "https://github.com/transitland/gtfs-archives-not-hosted-elsewhere/raw/master/island-transit-wa-us.zip",
               "http://www.mjcaction.com/MJC_GTFS_Public/jefferson_google_transit.zip",
               "http://developer.onebusaway.org/tmp/sound/gtfs/modified/1_gtfs.zip",
               "http://apps.kitsaptransit.com/google/latest/google_transit.zip",
               "http://www.mjcaction.com/MJC_GTFS_Public/lewismtnhwy_google_transit.zip",
               "http://data.trilliumtransit.com/gtfs/linktransit-wa-us/linktransit-wa-us.zip",
               "http://www.mjcaction.com/MJC_GTFS_Public/lccap_google_transit.zip",
               "http://data.trilliumtransit.com/gtfs/mason-wa-us/mason-wa-us.zip",
               "http://mjcaction.com/MJC_GTFS_Public/pacific_google_transit.zip",
               "http://www.mjcaction.com/MJC_GTFS_Public/pfp_google_transit.zip",
               "http://developer.onebusaway.org/tmp/sound/gtfs/modified/3_gtfs.zip",
               "http://data.trilliumtransit.com/gtfs/rivercitiestransit-wa-us/rivercitiestransit-wa-us.zip",
               "http://www.gtfs-data-exchange.com/agency/skagit-transit/latest.zip", 
               "http://developer.onebusaway.org/tmp/sound/gtfs/modified/40_gtfs.zip",
               "https://business.wsdot.wa.gov/Transit/csv_files/TranGO/trango.zip",
               "http://www.mjcaction.com/MJC_GTFS_Public/twintransit_google_transit.zip",
               "http://data.trilliumtransit.com/gtfs/uniongap-wa-us/uniongap-wa-us.zip",
               "http://www.mjcaction.com/MJC_GTFS_Public/valley_google_transit.zip",
               "http://www.mjcaction.com/MJC_GTFS_Public/wotm_google_transit.zip",
               "https://business.wsdot.wa.gov/Transit/csv_files/InterCityBus/WSDOT_GTFS_Intercity.zip",
               "http://mjcaction.com/MJC_GTFS_Public/yakima_google_transit.zip",
               "https://transitfeeds.com/p/spokane-transit-authority/199/latest/download",
               "https://github.com/whatcomtrans/publicwtadata/blob/master/GTFS/wta_gtfs_latest.zip?raw=true",
               "http://app.mecatran.com/urb/ws/feed/c2l0ZT10d2luLXRyYW5zaXQ7Y2xpZW50PXNlbGY7ZXhwaXJlPTt0eXBlPWd0ZnM7a2V5PTdjNWU2ZDVmMzYwNzc3MTM4MDcyNjQ2YzY2NjAxYTBlMmIwNjU0NzY=")


SaveName <- c('asotin.zip', # old end date in 2015
  'bft.zip',
  'clallam.zip',
  'community.zip', # Some old dates but they look fine
  'ctran.zip',
  'everett.zip',
  'grays_harbor.zip', # Some old dates but they look fine
  'intercity.zip',
  'island.zip',
  'jefferson.zip', # Some old dates but they look fine
  'king_county.zip',
  'kitsap.zip', # one old date looks fine
  'lewis.zip',
  'link.zip',
  'lower_columbia.zip',
  'mason.zip',
  'pacific.zip', 
  'people_for_people.zip',
  'pierce.zip',
  'rivercities.zip',
  'skagit.zip', # Old end date in 2017 set to 2020-01-01
  'sound_transit.zip',
  'trango.zip', # Some old dates but they look fine
  'twin.zip',
  'union_gap.zip',
  'valley.zip', # Some old dates but they look fine
  'wahkiakum.zip',
  'wsdot.zip',
  'yakima.zip',
  'spokane.zip',
  'whatcom.zip',
  'twin.zip')



#####
# Download and save
#####

DownloadGtfs <- function(url, saveName){
  zipFile <- httr::GET(url)
  t  <- httr::content(zipFile, as = "raw")
  writeBin(t, saveName)
}

mapply(DownloadGtfs, GtfsUrls, SaveName)

#####
# Validate dates
#####
ValidateDates <- function(GtfsFeed){
 
  #create temp dir, unzip, and convert text to dates
  dir.create(MyZip <- file.path(tempdir(), "MyZip"))
  unzip(GtfsFeed, exdir = MyZip)
  Calendar <- read.csv(file.path(MyZip, "calendar.txt"), stringsAsFactors = FALSE)
  Calendar$end_date <- as.Date(as.character(Calendar$end_date), format = "%Y%m%d")
  returnVal <- paste0(GtfsFeed, " - Passes date check")
  
  #Scans for posible date errors and ask users if they want to view them, then if they want to correct them
  if(any(Calendar$end_date < Sys.Date())){
    print(paste0(GtfsFeed,": Posible Error - Some end dates are before today."))
    correctErr <- readline(prompt = "Do you want to view potential errors? (y/n):")
    
    if(correctErr == "y"){
      View(Calendar)
      correctErr <- readline(prompt = "Do you want to correct errors? (y/n):")
      
      if(correctErr == "y"){
        # replaces all dates with the entered date
        NewDate <- readline(prompt = "Please enter a new end date (yyyymmdd example: 20200101):")
        Calendar$end_date <- NewDate
        
        #overwrite old zip with data from temp dir
        write.csv(Calendar, file.path(MyZip, "calendar.txt"), quote = FALSE, row.names = FALSE)
        file.remove(GtfsFeed)
        myWd <- getwd()
        setwd(MyZip)
        zip(file.path(myWd, GtfsFeed), files = list.files(MyZip))
        setwd(myWd)
      } else {
        returnVal <- paste0(GtfsFeed, " - Error: End date before today")
      }
    }
  }
  unlink(MyZip, recursive = T)
  returnVal
}

GtfsFeeds <- list.files(getwd())

lapply(GtfsFeeds, ValidateDates)





