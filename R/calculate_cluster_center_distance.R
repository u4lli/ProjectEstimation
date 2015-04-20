# setwd(getSrcDirectory(function(x) {x}))
# 
# if(!require("jsonlite")){
#   install.packages("jsonlite")
# }
# library(jsonlite)
# 
# if (!require("C50")) {
#   install.packages("C50", dependencies = TRUE)
# }
# library(C50)

# source("simpleR.R")

#' @export
loadProjData <- function(){
  projectdata <- get(load("../model/project_data_with_cluster.rda"))
  projectdata
}

# Convert project cluster center distance into percentage, so that it's easier to show on UI
#' @export
calculate_center_distance_as_percentage <- function(projectdata){
  projectdata$cluster_center_dist_as_percent <- 0
  
  clusters <- levels(projectdata$cluster)
  for(cluster in clusters){
    max_dist <- signlog(signlog(max(projectdata$cluster_center_dist2[projectdata$cluster == cluster])))
    projectdata$cluster_center_dist_as_percent[projectdata$cluster == cluster] <-
      round(signlog(signlog(projectdata$cluster_center_dist2[projectdata$cluster == cluster]))*100/max_dist, 2)
    }
  return (projectdata)
}

calcProjectDataAndSave <- function(projData){
  projectdata <- calculate_center_distance_as_percentage(projData)
  save(projectdata, file = "../model/project_data_with_cluster.rda")
}