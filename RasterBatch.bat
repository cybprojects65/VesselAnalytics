::####################################################
::######	Creazione Raster Customised	######
::####################################################

ogr2ogr -s_srs EPSG:4326 -t_srs EPSG:4326 -oo X_POSSIBLE_NAMES=xcentroid -oo Y_POSSIBLE_NAMES=ycentroid  -f "ESRI Shapefile" ratio.shp ratio.csv
ogr2ogr -s_srs EPSG:4326 -t_srs EPSG:4326 -oo X_POSSIBLE_NAMES=xcentroid -oo Y_POSSIBLE_NAMES=ycentroid  -f "ESRI Shapefile" reported_fishing.shp reported_fishing.csv
ogr2ogr -s_srs EPSG:4326 -t_srs EPSG:4326 -oo X_POSSIBLE_NAMES=xcentroid -oo Y_POSSIBLE_NAMES=ycentroid  -f "ESRI Shapefile" unreported_fishing.shp unreported_fishing.csv
ogr2ogr -s_srs EPSG:4326 -t_srs EPSG:4326 -oo X_POSSIBLE_NAMES=xcentroid -oo Y_POSSIBLE_NAMES=ycentroid  -f "ESRI Shapefile" total_fishing.shp total_fishing.csv

gdal_rasterize -l ratio -a ratio_unre -tr 0.011 0.011 -a_nodata -9999.0 -te 12.06522584 39.743097802 20.020827651 45.808913708 -ot Float32 -of GTiff ratio.shp ratio.tif
gdal_rasterize -l reported_fishing -a total_hour -tr 0.011 0.011 -a_nodata -9999.0 -te 12.06522584 39.743097802 20.020827651 45.808913708 -ot Float32 -of GTiff reported_fishing.shp reported_fishing.tif
gdal_rasterize -l unreported_fishing -a total_hour -tr 0.011 0.011 -a_nodata -9999.0 -te 12.06522584 39.743097802 20.020827651 45.808913708 -ot Float32 -of GTiff unreported_fishing.shp unreported_fishing.tif
gdal_rasterize -l total_fishing -a total_hour -tr 0.011 0.011 -a_nodata -9999.0 -te 12.06522584 39.743097802 20.020827651 45.808913708 -ot Float32 -of GTiff total_fishing.shp total_fishing.tif




::####################################################
::######	Creazione HeatMaps Customised	######
::####################################################


ogr2ogr -where intensity!='low' ratiohigh.shp ratio.shp
ogr2ogr -where intensity!='low' reported_fishinghigh.shp reported_fishing.shp
ogr2ogr -where intensity!='low' unreported_fishinghigh.shp unreported_fishing.shp
ogr2ogr -where intensity!='low' total_fishinghigh.shp total_fishing.shp


@echo off
call "C:\Program Files\QGIS 3.26.2\bin\qgis_process-qgis.bat"

qgis_process run qgis:heatmapkerneldensityestimation -- DECAY=0 INPUT=ratiohigh.shp KERNEL=0 OUTPUT=HeatMapRatio.tif OUTPUT_VALUE=0 PIXEL_SIZE=0.1 RADIUS=0.1 RADIUS_FIELD=  WEIGHT_FIELD=ratio_unre
qgis_process run qgis:heatmapkerneldensityestimation -- DECAY=0 INPUT=reported_fishinghigh.shp KERNEL=0 OUTPUT=HeatMapreported_fishing.tif OUTPUT_VALUE=0 PIXEL_SIZE=0.1 RADIUS=0.1 RADIUS_FIELD=  WEIGHT_FIELD=total_hour
qgis_process run qgis:heatmapkerneldensityestimation -- DECAY=0 INPUT=unreported_fishinghigh.shp KERNEL=0 OUTPUT=HeatMapunreported_fishing.tif OUTPUT_VALUE=0 PIXEL_SIZE=0.1 RADIUS=0.1 RADIUS_FIELD=  WEIGHT_FIELD=total_hour
qgis_process run qgis:heatmapkerneldensityestimation -- DECAY=0 INPUT=total_fishinghigh.shp KERNEL=0 OUTPUT=HeatMaptotal_fishing.tif OUTPUT_VALUE=0 PIXEL_SIZE=0.1 RADIUS=0.1 RADIUS_FIELD=  WEIGHT_FIELD=total_hour

gdaldem color-relief -alpha HeatMapRatio.tif colors.txt HeatMapRatioMagma.tif
gdaldem color-relief -alpha HeatMapreported_fishing.tif colors.txt HeatMapreported_fishingMagma.tif
gdaldem color-relief -alpha HeatMapunreported_fishing.tif colors.txt HeatMapunreported_fishingMagma.tif
gdaldem color-relief -alpha HeatMaptotal_fishing.tif colors.txt HeatMaptotal_fishingMagma.tif

del HeatMapRatio.tif
del HeatMapreported_fishing.tif
del HeatMapunreported_fishing.tif
del HeatMaptotal_fishing.tif

mkdir HeatMapsMagma Shapefiles Rasters

move HeatMapRatioMagma.tif HeatMapsMagma
move HeatMapreported_fishingMagma.tif HeatMapsMagma
move HeatMaptotal_fishingMagma.tif HeatMapsMagma
move HeatMapunreported_fishingMagma.tif HeatMapsMagma
move *.xml HeatMapsMagma

move total_fishing.tif Rasters
move ratio.tif Rasters
move reported_fishing.tif Rasters
move unreported_fishing.tif Rasters

move *.dbf Shapefiles
move *.shp Shapefiles
move *.shx Shapefiles
move *.prj Shapefiles