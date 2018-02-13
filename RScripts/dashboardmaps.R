#' dashboardmaps Creates a simple map that highlights a Colorado County or place
#'   Modified from cp_countymap  AB 2/2018
#'
#' This function creates a map to be used in the profile process,
#'    If a planning region is selected, the plannign region is colored in
#'    If a county is selected, the county is colored in and the planning region is outlined
#'    if a place is selected, the county is outlined and a dagger is posted at the center of the place.
#'
#'
#' @param fips is the fips code for the county to highlight
#'
cp_countymap=function(fips){

suppressPackageStartupMessages(require(rgdal))
suppressPackageStartupMessages(require(ggplot2))
suppressPackageStartupMessages(require(ggthemes))
suppressPackageStartupMessages(require(gridExtra))
suppressPackageStartupMessages(require(maptools))
suppressPackageStartupMessages(require(RCurl))

#Pulls the COunty Outlines
j=getURL("https://gis.dola.colorado.gov/capi/geojson?table=p1&sumlev=50&db=c2010&state=8&zoom=9")
gj=readOGR(j, "OGRGeoJSON", verbose=FALSE)
gj=fortify(gj)

#Pulls the County to Highlight
j1=getURL(paste0("https://gis.dola.colorado.gov/capi/geojson?table=p1&sumlev=50&db=c2010&state=8&zoom=9&county=", as.numeric(fips)))
gj1=readOGR(j1, "OGRGeoJSON", verbose=FALSE)
gj1=fortify(gj1)

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

return(m)
}
