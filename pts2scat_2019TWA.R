## Procedure to attach StreamCat variables to a list of points
# 3 major steps:
# A. Associate each point with the COMID from the NHD Plus (v2) catchment polygon it overlays geographically.
# B. Create 44 files, each containing a variable cluster of StreamCat data for the list of points. 
# C. Combine the 44 files into a single matrix of SiteID's (x-axis) by 418 StreamCat variables (y-axis) and export as csv.


##### Details...


##### A. Associate each point with the COMID from the NHD Plus (v2) catchment polygon it overlays geographically.

## Read and prep data

source('//share1.bluezone.usu.edu/miller/GIS/StreamCat/config_ModApp.R')

library(rgdal)
pts_list = read.csv( ptsFile )
pts = SpatialPointsDataFrame(coords = pts_list[, c('LONG','LAT')], data = pts_list, proj4string=(CRS('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0')))


## Map of points 

library(leaflet)
#Make leaflet map and add background
m = leaflet(pts) %>% 
  addProviderTiles("Esri.WorldShadedRelief", group="Terrain") %>% 
  addTiles(group="Streets")%>% 
  addProviderTiles("OpenTopoMap", group="Topo") %>% 
  addProviderTiles("Stamen.TonerLines", group="Political/Roads") %>% 
  addProviderTiles("Esri.WorldImagery", group="Imagery") %>%
  #Add points w/ popup labels
  addCircleMarkers(weight=5, col="black", opacity = 0.75, radius = 4) %>%
  #Add layer controls to the map
  addLayersControl(
    baseGroups = c('Terrain', 'Streets', 'Topo', 'Imagery'), #Set the baselayer 
    overlayGroups=c('Sites', 'Political/Roads'),
    options = layersControlOptions(collapsed = FALSE),
    position = 'bottomright'
  )
m




## Code to link points to hydrologic region

# Read in polygon of NHDPlus vector processing units (hydrologic regions)
# Drop points through polygons to match points with hydrologic region
# Loop through regions and extract COMIDs from shapefiles (takes a while)


#Read in polygons (attached in email)
vpus = readOGR(file.path(nhd_dir,'NHDPlusGlobalData'), 'VPUs')
#Extract data from polygon based on point locations
pts2 = over(pts, vpus)
#Add column of region and vpu IDs
pts$region_vpu = paste0(pts2$DrainageID, '_', pts2$UnitID)
#Find unique combinations of region and vpus IDs
regions = unique(pts$region_vpu)
#Loop through these combinations to read in the correct region
#Also selects just points for this region

# This typically takes ~5-10 minutes to run...
start.time.0 = Sys.time()
for(i in 1:length(regions)){
  print(regions[i])
  pts_tmp = pts[pts$region_vpu == regions[i],]
  #Split up unique region/vpu ID to use in path
  reg = strsplit(regions[i], '_')[[1]][[1]]
  vpu = strsplit(regions[i], '_')[[1]][[2]]
  #Define path
  cat_dir = paste0(nhd_dir,'/NHDPlus',reg,'/NHDPlus',vpu,'/NHDPlusCatchment')

  #Extract COMID from catchment shapefile
  pts_tmp$COMID = over(
    pts_tmp,
    readOGR(cat_dir, 'Catchment') #Read in polygon
  )$FEATUREID 
  #Combine tables into final output association table
  if(i == 1){
    out_df = pts_tmp@data
  }else{
    out_df = rbind(out_df, pts_tmp@data)
  }  
}
end.time.0 = Sys.time()
time.taken.0 = end.time.0 - start.time.0
time.taken.0 

str(out_df)
write.csv(out_df, paste0(wd1, '/output/pts_comid.csv'), row.names=F)


##### B. Create 44 files, each containing a variable cluster of StreamCat data for the list of points.


#Define directory paths

#Create a vector of StreamCat Variable Cluster (scvc) Names
scvcNames=list.dirs(file.path(sc_dir, 'HydroRegions'),full.names=FALSE)
# Store the file names of each StreamCat variable cluster in a list:  (this is super-fast)
scvcContents = list()
for(i in 1:length(scvcNames)){
  folder.path = file.path(sc_dir, 'HydroRegions',scvcNames[i])
  scvcContents[i] = list(list.files(path=folder.path,pattern = "*.csv"))
}
names(scvcContents) = scvcNames; 
head(scvcContents[2:length(scvcContents)])[1:2]



# Merge all the csv files within each StreamCat variable cluster and store the results in a list:
# This typically requires from 25 minutes to one hour to run.
scvcContentMerges = list()
time.taken = list()
from = 2
to = length(scvcContents)
start.time.1 = Sys.time()
for (i in from:to) { 
  start.time = Sys.time()
  setwd(file.path(sc_dir, 'HydroRegions',scvcNames[i]))
  content.merges =  do.call("rbind",lapply(scvcContents[[i]],read.csv)) # vertically combine all the files for a cluster variable
  scvcContentMerges[[i-1]] = merge(out_df,content.merges,by="COMID") # extract only the points and store that merged file in a list
  end.time = Sys.time()
  time.taken[[i]] = end.time - start.time
}
end.time.1 = Sys.time()
time.taken.1 = end.time.1 - start.time.1; time.taken.1

names(scvcContentMerges) = scvcNames[1:length(scvcContentMerges)+1]   
str(scvcContentMerges)

setwd(wd1)
if(saveInterimFile) save("scvcContentMerges",file = "scvcContentMerges.rdata");
#load(file = "scvcContentMerges.rdata")

# Merge StreamCat variable cluster files into a single file

# Make a vector of columns that repeat across files:
to.remove = c("COMID","Designatio","LAT","LONG","region_vpu","CatAreaSqKm","WsAreaSqKm","CatPctFull","WsPctFull")

# Remove repeated columns and combine all remaining columns
pts_StreamCat = scvcContentMerges[[1]]
for(i in 2:length(scvcContentMerges)){
  pts_StreamCat = cbind(pts_StreamCat, scvcContentMerges[[i]][,!(names(scvcContentMerges[[i]]) %in% names(pts_StreamCat))])
}
cat(ncol(pts_StreamCat)," Variables have been extracted for ",nrow(pts_StreamCat)," Sites.\n\n")

if(exists("outputVariables")){
  cat("Truncating output variables to match model requirements...")
  pts_StreamCat = pts_StreamCat[,names(pts_StreamCat) %in% outputVariables]
  cat("Complete!\n")
}
# Order columns alphabetically:
pts_StreamCat = pts_StreamCat[,order(names(pts_StreamCat))]

cat("Writing output file (",outputName,")...")
write.csv( pts_StreamCat, file.path(outputDirectory,outputName) ) 
cat("Complete!\n")
##write.csv(pts_StreamCat,"Z:/GIS/StreamCat/output/pts_StreamCatModAppZion.csv")

# Note: In Excel, you'll want to re-order the columns in the csv so that these 6 come first: SiteCode	COMID	Designatio	LAT	LONG	region_vpu

