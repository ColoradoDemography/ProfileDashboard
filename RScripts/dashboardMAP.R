#' dashboardMAP Creates a simple map that highlights a Colorado County or place
#'   Modified from cp_countymap  AB 2/2018
#'
#' This function creates a map to be used in the profile process,
#'    If a planning region is selected, the plannign region is colored in
#'    If a county is selected, the county is colored in and the planning region is outlined
#'    if a place is selected, the county is outlined and a dagger is posted at the center of the place.
#'
#'
#' @param ctyFips is the county fips code for the county to highlight
#' @param placefips is the place fips code for the map bullet
#' @export

dashboardMAP <- function(ctyfips,placefips,placelist){

  ctyfips <- ctyfips[order(ctyfips)]
  
  if(nchar(placefips) != 0){
    placefips <- substr(unique(placefips),3,7)
    # create a connection
    # save the password that we can "hide" it as best as we can by collapsing it
    pw <- {
      "demography"
    }
    
    # loads the PostgreSQL driver
    drv <- dbDriver("PostgreSQL")
    # creates a connection to the postgres database
    # note that "con" will be used later in each connection to the database
    con <- dbConnect(drv, dbname = "dola",
                     host = "104.197.26.248", port = 5433,
                     user = "codemog", password = pw)
    rm(pw) # removes the password
    
    f.muni <- dbGetQuery(con, paste0("SELECT placefp, x, y FROM bounds.place_centroids WHERE  placefp = '",placefips, "';"))
    
    #closing the connections
    dbDisconnect(con)
    dbUnloadDriver(drv)
    rm(con)
    rm(drv)
    
    f.muni$lat <- as.numeric(f.muni$y)
    f.muni$long <- as.numeric(f.muni$x)
    ctyx <- placelist[which(as.numeric(placefips) == placelist$placefips),]
    ctyfips <- str_pad(as.character(ctyx$countyfips),width=3,pad="0")
  }
  

 
  
  #Pulls the COunty Outlines
  j=getURL("https://gis.dola.colorado.gov/capi/geojson?table=p1&sumlev=50&db=c2010&state=8&zoom=9")
  gj=readOGR(j, "OGRGeoJSON", verbose=FALSE)
  gj=fortify(gj)
  
  gj1 <- data.frame()
  
  for(i in 1:length(ctyfips)) {
  #Pulls the County to Highlight
  j1 <- getURL(paste0("https://gis.dola.colorado.gov/capi/geojson?table=p1&sumlev=50&db=c2010&state=8&zoom=9&county=", as.numeric(ctyfips[i])))
  x <- readOGR(j1, "OGRGeoJSON", verbose=FALSE)
  x <- fortify(x)
  gj1 <- rbind(gj1,x)
  }

  
  m=ggplot()+
    geom_map(data=gj, map=gj,
             aes(x=long, y=lat, map_id=id),
             fill=rgb(239,239,239, max=255), color=rgb(92,102,112, max=255), size=.25)+
    geom_map(data=gj1, map=gj1,
             aes(x=long, y=lat, map_id=id),
             fill=rgb(0,149,58, max=255), color=rgb(92,102,112, max=255), size=.25)+
    coord_map(project="albers", lat0=40, lat1=39)+
    theme_map()+
    theme(panel.background=element_rect(fill=rgb(239,239,239, max=255), color=rgb(239,239,239, max=255)),
          plot.background=element_rect(fill=rgb(239,239,239, max=255), color=rgb(239,239,239, max=255)))
  
  if(nchar(placefips) != 0){  # Adding point for center of municipality
    m <- m + geom_point(data=f.muni, aes(x=x, y=y,shape="16", color="#655003c"),size=2) + 
      theme(legend.position="none")
  }
  
  return(m)
}