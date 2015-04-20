#setwd(getSrcDirectory(function(x) {x}))

if(!require("jsonlite")){
  install.packages("src/jsonlite_0.9.13.zip", lib = ".", repos = NULL, verbose = TRUE)
}
library(jsonlite)

if (!require("C50")) {
  install.packages("src/C50_0.1.0-19.zip", dependencies = TRUE, lib = ".", repos = NULL, verbose = TRUE)
}
library(C50)


# Functions
signlog <- function(x){sign(x) * log(sign(x)*x + 1)}
signlog_numeric <- function(x){ if (is.numeric(x)) signlog(x) else x }
inverse_signlog <- function(x){sign(x)*(exp(1)^(sign(x)*x) -1)}


projectdata <- get(load("project_data_with_cluster.rda"))
initial_predict_model <- get(load("initial_project_cluster_prediction_model.rda"))
apriori_predict_model <- get(load("apriori_project_cluster_prediction_model.rda"))
representative_projects <- get(load("representative_project_per_cluster.rda"))
project_cluster_summary <- get(load("project_summary_per_cluster.rda"))
project_cluster_statisctics <- get(load("project_clusters_statistics.rda"))
regression_model_total_amount <- get(load("reg_predict_total_amount_log_with_project_type.rda"))
regression_model_total_days <- get(load("reg_predict_total_days_log_with_project_type.rda"))


reg_model_file_name_total_amount_prefix <- "reg_predict_total_amount_log_with_project_type_"
reg_model_file_name_total_days_prefix <- "reg_predict_total_days_log_with_project_type_"

# cluster_count <- length(levels(projectdata$cluster))
# reg_model_matrix <- matrix(, nrow = cluster_count, ncol = 2)
# for(i in 1:cluster_count){
#   filename_total_amount = paste(reg_model_file_name_total_amount_prefix, i, ".rda", sep = "")
#   print(filename_total_amount)
#   filename_total_days = paste(reg_model_file_name_total_days_prefix, i, ".rda", sep = "")
#   reg_model_total_amount <- load(filename_total_amount)
#   reg_model_total_days <- load(filename_total_days)
#   print(i)
#   reg_model_pair <- cbind(reg_model_total_amount, reg_model_total_days)
#   reg_model_matrix[i,] <- reg_model_pair
# }


no_other_cluster_columns <- !(colnames(projectdata) %in% c("cluster_clara", "cluster_c5.0", "cluster_clara_c5.0"))

# Interface function

attach_customer_info <- function(input_data){

  customer_info <- head(subset(projectdata, customer == input_data$customer), 1L)
  if(!("customer_termsid" %in% names(input_data)))
    input_data$customer_termsid <- customer_info$customer_termsid
  if(!("customer_project_count" %in% names(input_data)))
    input_data$customer_project_count <- customer_info$customer_project_count
  input_data
}

attach_projmgr_info <- function(input_data){
  if("projmgr" %in% names(input_data)){
    projmgr_info <- head(subset(projectdata, projmgr == input_data$projmgr), 1L)
  }
  if(!("projmgr_inccat" %in% names(input_data))){
    input_data$projmgr_inccat <- projmgr_info$projmgr_inccat
  }
  if(!("projmgr_min_age" %in% names(input_data))){
    input_data$projmgr_min_age <- projmgr_info$projmgr_min_age
  }
  input_data
}

prepare_data_for_regression <- function(apriori_data){
  customer_info <- head(subset(projectdata, customer == apriori_data$customer), 1L)
  if(!("customer_project_count" %in% names(apriori_data)))
    apriori_data$customer_project_count <- customer_info$customer_project_count
  print(apriori_data)
  apriori_data <- lapply(apriori_data, signlog_numeric)
  data_frame <- data.frame(apriori_data)
  return(data_frame)
}

## Predict project cluster function
# Get input of the apriori variables, and predict the similar project cluster
# Get the JSON input, and return the project cluster number
predict_project_cluster_with_apriori_variables <- function(apriori_projectdata_json){
  apriori_data <- fromJSON(apriori_projectdata_json) 
  apriori_data <- attach_customer_info(apriori_data)
  apriori_data <- attach_projmgr_info(apriori_data)
  
  data_frame <- data.frame(apriori_data)
  predicted <- predict(apriori_predict_model, data_frame)
  return(predicted) 
}

predict_project_cluster_with_initial_variables <- function(initial_projectdata_json){
  initial_projectdata <- fromJSON(initial_projectdata_json)
  initial_projectdata <- attach_customer_info(initial_projectdata)
  data_frame <- data.frame(initial_projectdata)
  predicted <- predict(initial_predict_model, data_frame)
  return(predicted) 
}

predict_total_amount <- function(apriori_projectdata_json){
  apriori_data <- fromJSON(apriori_projectdata_json)
  data_frame <- prepare_data_for_regression(apriori_data)
  predicted <- predict(regression_model_total_amount, data_frame)
  print(predicted)
  return(inverse_signlog(predicted))
}

predict_total_days <- function(apriori_projectdata_json){
  apriori_data <- fromJSON(apriori_projectdata_json)
  data_frame <- prepare_data_for_regression(apriori_data)
  predicted <- predict(regression_model_total_days, data_frame)
  print(predicted)
  return(inverse_signlog(predicted))
}

predict_total_amount_and_days <- function(apriori_projectdata_json){
  apriori_data <- fromJSON(apriori_projectdata_json)
  data_frame <- prepare_data_for_regression(apriori_data)
  predicted_total_amount <- predict(regression_model_total_amount, data_frame)
  predicted_total_days <- predict(regression_model_total_days, data_frame)
  return(c(inverse_signlog(predicted_total_amount), inverse_signlog(predicted_total_days)))
  
}

predict_total_amount_in_cluster <- function(cluster_id, apriori_projectdata_json){
  apriori_data <- fromJSON(apriori_projectdata_json)
  data_frame <- prepare_data_for_regression(apriori_data)
  reg_model <- get(load(paste(reg_model_file_name_total_amount_prefix, cluster_id, ".rda", sep = "")))
  predicted <- predict(reg_model, data_frame)
  return(inverse_signlog(predicted))
}

predict_total_days_in_cluster <- function(cluster_id, apriori_projectdata_json){
  apriori_data <- fromJSON(apriori_projectdata_json)
  data_frame <- prepare_data_for_regression(apriori_data)
  reg_model <- get(load(paste(reg_model_file_name_total_days_prefix, cluster_id, ".rda", sep = "")))
  predicted <- predict(reg_model, data_frame)
  print(predicted)
  return(inverse_signlog(predicted))
}

predict_total_amount_and_days_in_cluster <-  function(cluster_id, apriori_projectdata_json){
  apriori_data <- fromJSON(apriori_projectdata_json)
  data_frame <- prepare_data_for_regression(apriori_data)
  reg_model_for_amount <- get(load(paste(reg_model_file_name_total_amount_prefix, cluster_id, ".rda", sep = "")))
  predicted_total_amount <- predict(reg_model_for_amount, data_frame)
  reg_model_for_days <- get(load(paste(reg_model_file_name_total_days_prefix, cluster_id, ".rda", sep = "")))
  predicted_total_days <- predict(reg_model_for_days, data_frame)
  return(c(inverse_signlog(predicted_total_amount), inverse_signlog(predicted_total_days)))
  
}

## Find the similar project id list for given cluster
# Get the input of the cluster id and the required number of projects. The type of the input parameters are int.
# The function returns a vector of project ids
get_similar_projects <- function(cluster_id, number_of_projects){
  first_nr_of_nearest_projects <- head(representative_projects[[cluster_id]], number_of_projects)
  return(first_nr_of_nearest_projects)
}

## Get the relevant projects with given filters. It sorts the cluster_center_dist2 column in project data.
get_relevant_projects <- function(cluster_id, number_of_projects = "all", filters = NULL){ 
  subset_data <- projectdata[projectdata$cluster == cluster_id, ]
  subset_data <- subset_data[with(subset_data, order(cluster_center_dist2)),]
  subset_str <- "subset_data"
  if(!is.null(filters)){
    subset_str <- paste("filter(", subset_str, ",", filters, ")", sep = "")  # remember to load dplyr library
  }
  
  subset_data <- eval(parse(text = subset_str))

  if(number_of_projects != "all")
    subset_data <- head(subset_data, number_of_projects)
  
  return(subset_data)
}


## Get the aggregated project information (a posteriori info) for the given project id
# Get the input of project id, and the data type is int.
# The function returns the a string with the project information
get_project_info <- function(project_id){
  
  project_info <- projectdata[projectdata$project == project_id, no_other_cluster_columns]
  return(project_info)
}

get_project_variable_names <- function(){
  return (colnames(projectdata[, no_other_cluster_columns]))
}

## Get the statistic summary of project cluster
# Get the input of cluster id, and the data type is int.
# The function returns the R list element  
get_project_cluster_summary <- function(cluster_id, out_format = "data.frame"){

  summary_table <- project_cluster_summary[[cluster_id]]
  if(out_format == "list"){
    summary_list <- list()
    col_names <- colnames(summary_table)
    col_length <- length(col_names)
    for(i in 1:col_length){
      key <- col_names[i]
      value <- summary_table[,i]
      summary_list[[key]] <- value
    }
    return(summary_list)
  }
  if(out_format == "data.frame"){
    return(as.data.frame.matrix(summary_table, make.unique(rownames(summary_table))))
  }
    
}

# Get the number of projects in given cluster
get_num_projects_in_cluster <- function(cluster_id){
  cluster_info <- project_cluster_statisctics[[cluster_id]] 
  cluster_col <- cluster_info[["cluster"]]
  return(cluster_col[names(cluster_col) == cluster_id])
}

# Get the statistics of each variable in given cluster
get_statistics_of_variable_in_cluster <- function(cluster_id, variable_name){
  cluster_info <- project_cluster_statisctics[[cluster_id]] 
  s_variable <- cluster_info[[variable_name]]
  s_names <- names(s_variable)
  s_values <- as.vector(s_variable)
  return(data.frame(cbind(s_names, s_values)))
}


# Get the quantile of total amount for each cluster with given probabilities 
get_quantile_of_total_amount <- function(cluster_id, probs = c(0.25, 0.75)){
  quantiled_data <- quantile(projectdata[projectdata$cluster == cluster_id,]$total_amount, probs)
  return(data.frame(cbind(names(quantiled_data), as.vector(quantiled_data))))
}

# Get the quantile of days count for each cluster with given probabilities 
get_quantile_of_days_count <- function(cluster_id, probs = c(0.25, 0.75)){
  quantiled_data <- quantile(projectdata[projectdata$cluster == cluster_id,]$days_count, probs)
  return(data.frame(cbind(names(quantiled_data), as.vector(quantiled_data))))
}

# Get the statistics of customer information 
get_customer_statistics <- function(customer_id, projmgr_id){
  customer_info <- subset(projectdata, customer == customer_id)
  
  customer_project_count <- nrow(customer_info)
  median_cost <- median(customer_info$total_amount)
  median_days <- median(customer_info$days_count)
  project_type_summary <- table(customer_info$project_type)
  
  customer_info <- subset(customer_info, projmgr == projmgr_id)
  
  pm_customer_project_count <- nrow(customer_info)
  pm_median_cost <- median(customer_info$total_amount)
  pm_median_days <- median(customer_info$days_count)
  pm_project_type_summary <- table(customer_info$project_type)
  
  return(data.frame(customer_project_count, median_cost, median_days, pm_customer_project_count,
                    pm_median_cost, pm_median_days))
}


# Get the project list of customer. If project manager id is given,
# it returns the list of projects for the given customer and project manager
get_customer_projects <- function(customer_id, projmgr_id = NULL){
  customer_info <- subset(projectdata, customer == customer_id)
  if(!is.null(projmgr_id)){
    customer_info <- subset(customer_info, projmgr == projmgr_id)
  }  
  return(customer_info$project)
}

# Get a vector of random ordered unique customers. The customers are from the top 20 customers in each cluster. 
get_representative_customers <- function(){
  cluster_no <- length(levels(projectdata$cluster))
  customer_list <- vector()
  for(i in 1:cluster_no){
    subset_data <- projectdata[projectdata$cluster == i, ]
    customers_frequency <- table(subset_data$customer)
    customers_frequency_in_order <- head(customers_frequency[order(customers_frequency, decreasing = T)], 20)
    customer_list <- append(customer_list, names(customers_frequency_in_order))
  }
  customer_list <- unique(customer_list)
  set.seed(10)
  customer_list <- sample(customer_list)
  return(customer_list)
}

# Convert the customer vector into json format. This is just called in the console and copy the result to the json file.
produce_customer_json <- function(){
  id <- get_representative_customers()
  description <- paste("Customer", id, sep = " ")
  customer_dataframe <- data.frame(id, description)
  return(toJSON(customer_dataframe))
}

# Get a vector of random ordered unique departments. The departments are from the top 50 customers in each cluster. 
get_representative_departments <- function(){
  cluster_no <- length(levels(projectdata$cluster))
  department_list <- vector()
  for(i in 1:cluster_no){
    subset_data <- projectdata[projectdata$cluster == i, ]
    departments_frequency <- table(subset_data$department)
    departments_frequency_in_order <- head(departments_frequency[order(departments_frequency, decreasing = T)], 50)
    department_list <- append(department_list, names(departments_frequency_in_order))
  }
  department_list <- unique(department_list)
  set.seed(10)
  department_list <- sample(department_list)
  return(department_list)
}

# Convert the department vector into json format. This is just called in the console and copy the result to the json file.
produce_department_json <- function(){
  id <- get_representative_departments()
  description <- paste("Department", id, sep = " ")
  department_dataframe <- data.frame(id, description)
  return(toJSON(department_dataframe))
}