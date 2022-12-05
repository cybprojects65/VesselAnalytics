rm(list=ls(all=TRUE))
library(raster)
library(sf)
library(robis)

output_file<-"stocks_involved.csv"

rasterfile<-raster("HeatMapunreported_fishingHigh.tif")
#get the boundaries
min_x_in_raster<-rasterfile@extent[1]
max_x_in_raster<-rasterfile@extent[2]
min_y_in_raster<-rasterfile@extent[3]
max_y_in_raster<-rasterfile@extent[4]
resolution = res(rasterfile)[1]
boundingbox = paste0("POLYGON ((",
                     min_x_in_raster," ",min_y_in_raster,", ",
                     min_x_in_raster," ",max_y_in_raster,", ",
                     max_x_in_raster," ",max_y_in_raster,", ",
                     max_x_in_raster," ",min_y_in_raster,", ",
                     min_x_in_raster," ",min_y_in_raster,
                     "))")

#build an x,y,value grid at the given resolution
xseq<-seq(from=min_x_in_raster,to=max_x_in_raster,by=resolution)
yseq<-seq(from=min_y_in_raster,to=max_y_in_raster,by=resolution)
grid_of_points<-expand.grid(x = xseq, y = yseq)#combine the x and y coordinate to generate pairs
grid_values<-extract(x=rasterfile,y=grid_of_points,method='simple')
grid_of_points$values<-grid_values
grid_of_points<-grid_of_points[-which(is.na(grid_of_points$values)),]
plot(density(grid_of_points$values))
#take the points over the 75th percentile of the values = high values
q75<-as.numeric(quantile(grid_of_points$values,probs=c(0.75)))
grid_of_points_high_values<-grid_of_points[-which(grid_of_points$values>=q75),]
#create a WKT version of the points (will be later intersected) with the FAO points
grid_of_points_high_values$points_wkt<-paste0("POINT(",grid_of_points_high_values$x," ",grid_of_points_high_values$y,")")
grid_of_points_high_values$polygons<-paste0("((",
                                            grid_of_points_high_values$x-(resolution/2)," ",grid_of_points_high_values$y-(resolution/2),", ",
                                            grid_of_points_high_values$x-(resolution/2)," ",grid_of_points_high_values$y+(resolution/2),", ",
                                            grid_of_points_high_values$x+(resolution/2)," ",grid_of_points_high_values$y+(resolution/2),", ",
                                            grid_of_points_high_values$x+(resolution/2)," ",grid_of_points_high_values$y-(resolution/2),", ",
                                            grid_of_points_high_values$x-(resolution/2)," ",grid_of_points_high_values$y-(resolution/2),
                                            "))")


#multipoly<-paste0("MULTIPOLYGON (",paste(grid_of_points_high_values$polygons,collapse = ","),")")

#get the really observed species in the bounding box
speciesinbb="speciesobis.Rdata"
if (!file.exists(speciesinbb)){
  speciesOBIS<-occurrence(geometry=boundingbox) 
  #IUCN<-checklist(geometry=boundingbox,redlist=TRUE)
  save(list = c("speciesOBIS"),file=speciesinbb)
}else{
  load(file = speciesinbb)
}

point_geometries<-st_as_sfc(grid_of_points_high_values$points_wkt)

#select the stocks with a geometry
FAOlist<-read.delim("ASFIS_sp_2022_geom.txt",header=T,sep=",",encoding="UTF-8")
FAOlist<-FAOlist[,c("TAXOCODE","Scientific_name","geometry")]
names(FAOlist)[names(FAOlist) == "Scientific_name"] <- "scientificName"
names(FAOlist)[names(FAOlist) == "geometry"] <- "geometries"
FAOlist<-FAOlist[-which(nchar(FAOlist$geometries)==0),]

#cast geometries to ST objects
FAOlist_SF<-st_as_sf(FAOlist,wkt="geometries")
FAOlist_SF %>% st_cast()
fao_geometries<-FAOlist_SF$geometries

stocks<-c()
intersections<-c()
geomcounter<-1

#for each geometry, intersect it with the grid points
for (sp_geom in fao_geometries){
    
  intersection<-st_as_text(st_intersection(sp_geom,point_geometries))
  #if the point falls in the geometry, annotate the stock as present in the fishing area
  if (nchar(intersection)>0 && !grepl("EMPTY",intersection)){
    stocks<-c(stocks, FAOlist_SF$scientificName[geomcounter])    
    intersections<-c(intersections,intersection)
  }
  geomcounter<-geomcounter+1  
}

#select the marine species in obis
speciesOBIS=speciesOBIS[speciesOBIS$marine=="TRUE",]
#select the fao stocks present and observed in the area
stocks_observed<-sort(stocks[which(stocks %in% unique(speciesOBIS$scientificName))])
#select the fao stocks that are present and observed in the area and are in the IUCN red list
threatened_species<-unique(speciesOBIS$scientificName[which( (speciesOBIS$category == 'VU') | 
                                                        (speciesOBIS$category == 'EN') | 
                                                        (speciesOBIS$category == 'CR')
                                                      )])
stocks_redlist<-sort(stocks[which(stocks %in% threatened_species)])
stocks_observed_df<-data.frame(scientific_names=stocks_observed)
stocks_observed_df$is_threatened<-F
stocks_observed_df$is_threatened[which(stocks_observed %in% stocks_redlist)]<-T

write.csv(stocks_observed_df,output_file,row.names = F)

