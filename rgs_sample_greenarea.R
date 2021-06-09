# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#  Copyright 2016-2019 University of Melbourne
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
# 
# Written by: Dr. Yiqun Chen    yiqun.c@unimelb.edu.au
# 
# DevLogs:

# v1.4 2021-06-09
# (1) R v4.1.0 tested
#
# v1.3 2020-06-24
# (1) update greenarea and population sample data url
# (2) update utils.publishSP2GeoServerWithMultiStyles function parameters
# (3) R v4.0.0 tested
#
# v1.2 2018-07-18
# (1) add layerprefix and styleprefix for the outputs 
#
# v1.1 2018-07-15
# (1) update urls 
#
# v1.0 2017-07-18
# (1) init commitment 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

library(maptools) 
library(rgdal)
library(rgeos)
library(jsonlite)
library(doParallel)

# using 4 cores for parallel computing
registerDoParallel(cores=4) 

# change working directory to your own dir path where the r-geoserver.zip is unzipped to
setwd("E:\\02_Teaching\\r-geoserver")

# load utils methods for use
source("rgs_utils.R")

# calcuate green area index for Melbourne Inner region using SA1 population (Census 2016 ABS) and greenspace data (2019 PSMA)
execIndicatorGreenArea <- function(){
  
  # the follow two lines are for testing
  greenarea_wfsurl = "https://ds2.digitwin.com.au:8443/geoserver/ows?service=WFS&request=GetFeature&version=1.0.0&typeName=livedata:greenspace_psma2019_sa4_melbourne_inner&outputFormat=json"
  pop_wfsurl = "https://ds2.digitwin.com.au:8443/geoserver/ows?service=WFS&request=GetFeature&version=1.0.0&typeName=livedata:population_abs2016_sa1_melbourne_inner&outputFormat=json"
  
  # load spatial object direct from geojson
  sp_greenarea = utils.loadGeoJSON2SP(greenarea_wfsurl)
  # check if data layer can be successfully loaded
  if(is.null(sp_greenarea)){
    utils.debugprint("fail to load data layer for greenarea")
    return(FALSE)
  }
  
  sp_pop = utils.loadGeoJSON2SP(pop_wfsurl)
  # check if data layer can be successfully loaded
  if(is.null(sp_pop)){
    utils.debugprint("fail to load data layer for population")
    return(FALSE)
  }
  
  pop_basenum = 100000 # green area index calculation base is for 100,000 persons
  
  # # # # # # # # # # # # # 
  # implement indicator logic here, such as
  # 1. spatial data operations like projection, intersection, union, 
  # 2. statistics generation
  # 3. etc.
  # # # # # # # # # # # # # 
  
  # project(transform) sp into UTM to enable area calculation
  sp_greenarea_prj = utils.project2UTM(sp_greenarea)
  
  # # # # # # # # # # # # # # # # # # # # # # 
  # calculation greenarea for each population polygon 
  # # # # # # # # # # # # # # # # # # # # # # 
  
  # project(transform) sp into UTM to enable area calculation, since the orginal population polygin in WGS84 coordinate reference system
  sp_pop_prj = utils.project2UTM(sp_pop)
  
  # add two more attributes for sp_pop_prj, one is for the actual size of greenarea, the other is for the greenarea index
  sp_pop_prj@data[,"gaarea"] = 0.0
  sp_pop_prj@data[,"idxval"] = 0.0
  
  # ==== main loop starts here ====
  # for each population polygon, find all green areas it intersects and calcuate the size of intersected area
  # two methods are implemented:
    
  # ===================================
  # method 1 : using parallel computing
  # ===================================
  
  # bulid foreach result as a matrix containing 3 columns storing values for gaarea, idxval and mb_code11 respectively
  result = foreach(i=1:nrow(sp_pop_prj), .combine = rbind, .export=c("SpatialPolygons","over","gIntersection","gArea","gIsValid")) %dopar% {

    # get the geometry polgyon of population, return 0 for gaarea and idxval if geometry is NULL
    if(is.null(sp_pop_prj@polygons[i])){
      out = c(0, 0)
    }else{
      
      err_occur <- FALSE
      
      tryCatch({

          geom_pop = SpatialPolygons(sp_pop_prj@polygons[i], proj4string=sp_pop_prj@proj4string)
    
          # accumulate the total size of intersected greenarea for the current population geometry
          intersectedGreenArea = 0.0
    
          # this 'over' method is much faster to find all intersected green area polygons of current pop polygon
          # temporarily save all intersected greenarea into a sub spatialdataframe
          intersectedGADF = sp_greenarea_prj[!is.na(over(sp_greenarea_prj,sp_pop_prj[i,]))[,1],]
    
          # if intersected with one or more greenarea polygon, calculate and accumulate the intersected area for each population SA1
          if(nrow(intersectedGADF)>0){
    
            for(j in nrow(intersectedGADF):1){
    
              geom_greenarea = SpatialPolygons(intersectedGADF@polygons[j], proj4string=intersectedGADF@proj4string)
    
              # do the actual intersction process
              intsectedGeom = gIntersection(geom_pop, geom_greenarea)
              # accumulate the size of intersected greenarea
              intersectedGreenArea = intersectedGreenArea + gArea(intsectedGeom)
    
            }
          }
    
          # check population attribute, make sure it is valid
          population = sp_pop_prj@data[i,"total"]
    
          if(is.null(population)||is.na(population)) population=0
    
          # for those polygons with 0 population, assign idxval = 0
          idx_val = 0
          if(population>0){
            idx_val = intersectedGreenArea / (population / (pop_basenum * 1.0))
          }
    
          out = c(intersectedGreenArea, idx_val)
      }, 
      
      error = function(e) {
        #utils.debugprint(sprintf("err: %s", e))
        out = c(0, 0)
      }
      )
      
    }
  }

  # assign calculated values back to sp_pop_prj@data. use as.numberic() to assure the values are numeric
  sp_pop_prj@data[,"gaarea"] = as.numeric(result[,1])
  sp_pop_prj@data[,"idxval"] = as.numeric(result[,2])

  
  # ===================================
  # method 2: using normal for loop
  # ===================================
  
  # this process takes long time to accomplish. 
  # in RStudio, use Ctrl+Shift+C to uncomment/comment it for testing
  
  
  # for(i in nrow(sp_pop_prj):1){
  # 
  #   utils.debugprint(sprintf("processing [%i/%i]", i, nrow(sp_pop_prj)))
  # 
  #   # get the geometry polgyon of population, skip if it is NULL
  #   if(is.null(sp_pop_prj@polygons[i])){
  #     next
  #   }
  # 
  #   skip_to_next <- FALSE
  # 
  #   tryCatch({
  # 
  #   geom_pop = SpatialPolygons(sp_pop_prj@polygons[i], proj4string=sp_pop_prj@proj4string)
  # 
  #   # accumulate the total size of intersected greenarea for the current population geometry
  #   intersectedGreenArea = 0.0
  # 
  #   # this 'over' method is much faster to find all intersected green area polygons of current pop polygon
  #   # temporarily save all intersected greenarea into a sub spatialdataframe
  #   intersectedGADF = sp_greenarea_prj[!is.na(over(sp_greenarea_prj,sp_pop_prj[i,]))[,1],]
  # 
  #   # if intersected with one or more greenarea polygon, calculate and accumulate the intersected area for each population SA1
  #   if(nrow(intersectedGADF)>0){
  # 
  #     for(j in nrow(intersectedGADF):1){
  # 
  #       geom_greenarea = SpatialPolygons(intersectedGADF@polygons[j], proj4string=intersectedGADF@proj4string)
  # 
  #       # do the actual intersction process
  #       intsectedGeom = gIntersection(geom_pop, geom_greenarea)
  #       # accumulate the size of intersected greenarea
  #       intersectedGreenArea = intersectedGreenArea + gArea(intsectedGeom)
  # 
  #     }
  #   }
  # 
  #   # check population attribute, make sure it is valid
  #   population = sp_pop_prj@data[i,"total"]
  # 
  #   if(is.null(population)||is.na(population)) population=0
  # 
  #   # for those polygons with 0 population, assign idxval = 0
  #   if(population>0){
  #     sp_pop_prj@data[i,"idxval"] = intersectedGreenArea / (population / (pop_basenum * 1.0))
  #   }
  #   # assgin intersectedGreenArea to gaarea attribute
  #   sp_pop_prj@data[i,"gaarea"] = intersectedGreenArea
  # 
  #   },
  #   error = function(e) {
  #     utils.debugprint(sprintf("err: %s", e))
  #     
  #     skip_to_next <<- TRUE}
  #   )
  # 
  # 
  #   if(skip_to_next) { next }
  # }

  
  # ==== main loop ends here ====
  

  
  
  # this example shows how to publish a geolayer by creating two wms styles on various attributes of the same data layer. 
  # the data layer will be only published one time, with various wms styles generated for selected attributes 
  publishedinfo = utils.publishSP2GeoServerWithMultiStyles(spobj=sp_pop_prj, 
                                                           layerprefix="greenarea_",
                                                           styleprefix="greenarea_stl_",
                                                           attrname_vec=c("gaarea","idxval"),
                                                           layerdisplyname_vec=c("green area index by area", "green area index by index value"),
                                                           palettename_vec=c("Reds","Blues"), 
                                                           colorreverseorder_vec=c(FALSE,FALSE), 
                                                           colornum_vec=c(6,8), 
                                                           classifier_vec=c("Jenks","Jenks"),
                                                           bordercolor_vec=c("black", "gray"),
                                                           borderwidth_vec=c(1, 2),
                                                           bordervisible_vec=c(TRUE, TRUE),
                                                           styletype_vec=c("graduated","graduated")
                                                          )
  
  if(is.null(publishedinfo) || length(publishedinfo)==0){
    utils.debugprint("fail to save data to geoserver")
    return(FALSE)
  }
  
  # set the second style (by idxval, Blues) as the default style for the output layer
  utils.addDefaultStyleToDataLayer(stylename=publishedinfo[[2]]$wms$styleparams$stylename, datalayername=publishedinfo[[2]]$layername)
  
  # print the outputs in json format
  utils.debugprint(sprintf("outputs: %s", toJSON(publishedinfo, auto_unbox=TRUE)))
  

  return(TRUE)
}

execIndicatorGreenArea()