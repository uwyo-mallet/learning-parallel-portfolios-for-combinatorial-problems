cluster = NaN
learnertype = "ranger"
benchmark_name = "IPC2018"
se.method = "jack"
combined = "seperate" 
plot_title = "IPC2018"

assign_jp_optimum = function(learnertype, se.method, combined) {
  #should be updated:
  # previous results >> include all | including TO
  # ranger jack
  # jp_optimum = 0.33
  # ranger infjack
  # jp_optimum = 0.44
  # randomForest 
  # jp_optimum = 0.59

  # Define a nested list (acting as a dictionary) with the jp_optimum values
  jp_values <- list(
    combined = list(
      ranger = list(
        jack = 0.65,
        infjack = 0.56
      ),
      randomForest = 0.55
    ),
    seperate = list(
      ranger = list(
        jack = 0.27,
        infjack = 0.44
      ),
      randomForest = 0.74
    )
  )
  
  if (!combined %in% names(jp_values)) {
    stop("Invalid 'combined' value. Use 'combined' or 'seperate'.")
  }
  
  if (!learnertype %in% names(jp_values[[combined]])) {
    stop(paste("Learnertype", learnertype, "not recognized."))
  }
  
  learner_info <- jp_values[[combined]][[learnertype]]
  
  if (is.list(learner_info) && se.method != "") {
    if (!se.method %in% names(learner_info)) {
      stop(paste("se.method", se.method, "not found for learnertype", learnertype))
    }
    jp_optimum <- learner_info[[se.method]]
  } else {
    jp_optimum <- learner_info # For methods without sub-methods or when se.method is not applicable
  }
  
  return(jp_optimum)
}

# jp_optimum = assign_jp_optimum(learnertype, se.method, combined)

assign_kl_optimum = function(learnertype, se.method, combined) {
  # Define a nested list (acting as a dictionary) with the kl_optimum values
  kl_values <- list(
    combined = list(
      ranger = list(
        jack = 0.52,
        infjack = 0.89
      ),
      randomForest = 1
    ),
    seperate = list(
      ranger = list(
        jack = 0.93,
        infjack = 0.93
      ),
      randomForest = 0.71
    )
  )
  
  if (!combined %in% names(kl_values)) {
    stop("Invalid 'combined' value. Use 'combined' or 'seperate'.")
  }
  
  if (!learnertype %in% names(kl_values[[combined]])) {
    stop(paste("Learnertype", learnertype, "not recognized."))
  }
  
  learner_info <- kl_values[[combined]][[learnertype]]
  
  if (is.list(learner_info) && se.method != "") {
    if (!se.method %in% names(learner_info)) {
      stop(paste("se.method", se.method, "not found for learnertype", learnertype))
    }
    kl_optimum <- learner_info[[se.method]]
  } else {
    kl_optimum <- learner_info # For methods without sub-methods or when se.method is not applicable
  }
  
  return(kl_optimum)
}

# kl_optimum = assign_kl_optimum(learnertype, se.method, combined)

# jp_average_optimum = 0.82
max_core = 10

create_directory_if_not_exist <- function(directory_path) {
  if (!file.exists(directory_path)) {
    dir.create(directory_path, recursive = TRUE)
    cat("Directory created:", directory_path, "\n")
  } else {
    cat("Directory already exists:", directory_path, "\n")
  }
}

set_paths = function(){
  if(Sys.info()['sysname'][[1]]=="Linux"){
    if(file.exists("~/Documents/nextSteps/Scripts/OverheadResults.R")){
      source("~/Documents/nextSteps//Scripts/OverheadResults.R")
      basepath <<- paste0("~/Documents/ranger_results_",combined,"/","/results/",benchmark_name,"/", learnertype, "/", se.method,"/")
      create_directory_if_not_exist(basepath)
      predictionPath <<- paste0("~/Documents/ranger_results_",combined,"/","/",benchmark_name,"/", learnertype, "/", se.method,"/")
      modelPath <<- paste0("~/Documents/mlr-scripts/",benchmark_name,"/Prediction/StandardError/")
      cluster <<- FALSE
      top_vbs_path <<- paste0("~/Documents/ranger_results_",combined,"/","/results/",benchmark_name,"/",learnertype, "/", se.method,"/top_vbs.csv")
      top_sbs_path <<- paste0("~/Documents/ranger_results_",combined,"/","/results/",benchmark_name,"/",learnertype, "/", se.method,"/top_sbs.csv")
      selectionPath_as_no_uncertainty <<- paste0("~/Documents/ranger_results_",combined,"/","/results/",benchmark_name,"/",learnertype, "/", se.method,"/selections_algorithmSelection_noUncertainty/")
      create_directory_if_not_exist(selectionPath_as_no_uncertainty)
      top_as_noUncertainty_path <<- paste0("~/Documents/ranger_results_",combined,"/","/results/",benchmark_name,"/",learnertype, "/", se.method,"/top_as_noUncertainty_")
      timesplitting_schedule_path <<- paste0("~/Documents/ranger_results_",combined,"/","/results/",benchmark_name,"/",learnertype, "/", se.method,"/time_splitting/")
      create_directory_if_not_exist(timesplitting_schedule_path)
      timesplitting_se_schedule_path <<- paste0("~/Documents/ranger_results_",combined,"/","/results/",benchmark_name,"/",learnertype, "/", se.method, "/time_splitting/pred+SE/")
      create_directory_if_not_exist(timesplitting_se_schedule_path)
      flexfolio_3s_path <<- paste0("~/Documents/nextSteps/Baselines/flexfolio/", benchmark_name,"/", benchmark_name,"_results/",benchmark_name,"-")
      joint_probability_path <<- paste0("~/Documents/ranger_results_",combined,"/","/results/",benchmark_name,"/",learnertype, "/", se.method,"/JointProbability/")
      kl_div_path <<- paste0("~/Documents/ranger_results_",combined,"/","/results/",benchmark_name,"/",learnertype, "/", se.method,"/KL_div/")
      create_directory_if_not_exist(joint_probability_path)
      create_directory_if_not_exist(kl_div_path)
      jp_optimum_path <<- paste0("~/Documents/ranger_results_",combined,"/","/results/",benchmark_name,"/",learnertype, "/", se.method,"/joint_optimal/")
      create_directory_if_not_exist(jp_optimum_path)
      kl_optimum_path <<- paste0("~/Documents/ranger_results_",combined,"/","/results/",benchmark_name,"/",learnertype, "/", se.method,"/kl_div_optimal/")
      create_directory_if_not_exist(kl_optimum_path)
      jp_optimum_avg_path <<- paste0("~/Documents/ranger_results_",combined,"/","/results/",benchmark_name,"/",learnertype, "/", se.method,"/joint_optimal_avg/")
      create_directory_if_not_exist(jp_optimum_avg_path)
    } else{
      source("/gscratch/hkashgar/nextSteps/Scripts/OverheadResults.R")
      basepath <<- paste0("/gscratch/hkashgar/ranger_results_",combined,"/","/results/",benchmark_name,"/", learnertype, "/", se.method,"/")
      create_directory_if_not_exist(basepath)
      predictionPath <<- paste0("/gscratch/hkashgar/ranger_results_",combined,"/","/",benchmark_name,"/", learnertype, "/", se.method,"/")
      modelPath <<- paste0("/gscratch/hkashgar/mlr-scripts/",benchmark_name,"/Prediction/StandardError/")
      cluster <<- TRUE
      top_vbs_path <<- paste0("/gscratch/hkashgar/ranger_results_",combined,"/","/results/",benchmark_name,"/",learnertype, "/", se.method,"/top_vbs.csv")
      top_sbs_path <<- paste0("/gscratch/hkashgar/ranger_results_",combined,"/","/results/",benchmark_name,"/",learnertype, "/", se.method,"/top_sbs.csv")
      selectionPath_as_no_uncertainty <<- paste0("/gscratch/hkashgar/ranger_results_",combined,"/","/results/",benchmark_name,"/",learnertype, "/", se.method,"/selections_algorithmSelection_noUncertainty/")
      create_directory_if_not_exist(selectionPath_as_no_uncertainty)
      top_as_noUncertainty_path <<- paste0("/gscratch/hkashgar/ranger_results_",combined,"/","/results/",benchmark_name,"/",learnertype, "/", se.method,"/top_as_noUncertainty_")
      timesplitting_schedule_path <<- paste0("/gscratch/hkashgar/ranger_results_",combined,"/","/results/",benchmark_name,"/",learnertype, "/", se.method,"/time_splitting/")
      create_directory_if_not_exist(timesplitting_schedule_path)
      timesplitting_se_schedule_path <<- paste0("/gscratch/hkashgar/ranger_results_",combined,"/","/results/",benchmark_name,"/",learnertype, "/", se.method,"/time_splitting/pred+SE/")
      create_directory_if_not_exist(timesplitting_se_schedule_path)
      flexfolio_3s_path <<- paste0("/gscratch/hkashgar/nextSteps/Baselines/flexfolio/",benchmark_name,"/",benchmark_name,"_results/",benchmark_name,"-")
      joint_probability_path <<- paste0("/gscratch/hkashgar/ranger_results_",combined,"/","/results/",benchmark_name,"/",learnertype, "/", se.method,"/JointProbability/") 
      kl_div_path <<- paste0("/gscratch/hkashgar/ranger_results_",combined,"/","/results/",benchmark_name,"/",learnertype, "/", se.method,"/KL_div/")
      create_directory_if_not_exist(joint_probability_path)
      create_directory_if_not_exist(kl_div_path)
      jp_optimum_path <<- paste0("/gscratch/hkashgar/ranger_results_",combined,"/","/results/",benchmark_name,"/",learnertype, "/", se.method,"/joint_optimal/")
      create_directory_if_not_exist(jp_optimum_path)
      kl_optimum_path <<- paste0("/gscratch/hkashgar/ranger_results_",combined,"/","/results/",benchmark_name,"/",learnertype, "/", se.method,"/kl_div_optimal/")
      create_directory_if_not_exist(kl_optimum_path)
      jp_optimum_avg_path <<- paste0("/gscratch/hkashgar/ranger_results_",combined,"/","/results/",benchmark_name,"/",learnertype, "/", se.method,"/joint_optimal_avg/")
      create_directory_if_not_exist(jp_optimum_avg_path)
    }
    
  } else{
    source("C:/Users/hnyk9/Thesis/nextSteps/Scripts/OverheadResults.R")
    basepath <<- paste0("C:/Users/hnyk9/Thesis/ranger_results_",combined,"/","/results/",benchmark_name,"/", learnertype, "/", se.method,"/")
    create_directory_if_not_exist(basepath)
    predictionPath <<- paste0("C:/Users/hnyk9/Thesis/ranger_results_",combined,"/","/",benchmark_name,"/", learnertype, "/", se.method,"/")
    modelPath <<- paste0("C:/Users/hnyk9/Thesis/mlr-scripts/Prediction/",benchmark_name,"/StandardError/")
    cluster <<- FALSE
    top_vbs_path <<- paste0("C:/Users/hnyk9/Thesis/ranger_results_",combined,"/","/results/",benchmark_name,"/",learnertype, "/", se.method,"/top_vbs.csv")
    top_sbs_path <<- paste0("C:/Users/hnyk9/Thesis/ranger_results_",combined,"/","/results/",benchmark_name,"/",learnertype, "/", se.method,"/top_sbs.csv")
    selectionPath_as_no_uncertainty <<- paste0("C:/Users/hnyk9/Thesis/ranger_results_",combined,"/","/results/",benchmark_name,"/",learnertype, "/", se.method,"/selections_algorithmSelection_noUncertainty/")
    create_directory_if_not_exist(selectionPath_as_no_uncertainty)
    top_as_noUncertainty_path <<- paste0("C:/Users/hnyk9/Thesis/ranger_results_",combined,"/","/results/",benchmark_name,"/",learnertype, "/", se.method,"/top_as_noUncertainty_")
    timesplitting_schedule_path <<- paste0("C:/Users/hnyk9/Thesis/ranger_results_",combined,"/","/results/",benchmark_name,"/",learnertype, "/", se.method,"/time_splitting/")
    create_directory_if_not_exist(timesplitting_schedule_path)
    timesplitting_se_schedule_path <<- paste0("C:/Users/hnyk9/Thesis/ranger_results_",combined,"/","/results/",benchmark_name,"/",learnertype, "/", se.method,"/time_splitting/pred+SE/")
    create_directory_if_not_exist(timesplitting_se_schedule_path)
    flexfolio_3s_path <<- paste0("C:/Users/hnyk9/Thesis/nextSteps/Baselines/flexfolio/",benchmark_name,"/",benchmark_name,"_results/",benchmark_name,"-")
    joint_probability_path <<- paste0("C:/Users/hnyk9/Thesis/ranger_results_",combined,"/","/results/",benchmark_name,"/",learnertype, "/", se.method,"/JointProbability/") 
    kl_div_path <<- paste0("C:/Users/hnyk9/Thesis/ranger_results_",combined,"/","/results/",benchmark_name,"/",learnertype, "/", se.method,"/KL_div/")
    create_directory_if_not_exist(joint_probability_path)
    create_directory_if_not_exist(kl_div_path)
    jp_optimum_path <<- paste0("C:/Users/hnyk9/Thesis/ranger_results_",combined,"/","/results/",benchmark_name,"/",learnertype, "/", se.method,"/joint_optimal/")
    create_directory_if_not_exist(jp_optimum_path)
    kl_optimum_path <<- paste0("C:/Users/hnyk9/Thesis/ranger_results_",combined,"/","/results/",benchmark_name,"/",learnertype, "/", se.method,"/kl_div_optimal/")
    create_directory_if_not_exist(kl_optimum_path)
    jp_optimum_avg_path <<- paste0("C:/Users/hnyk9/Thesis/ranger_results_",combined,"/","/results/",benchmark_name,"/",learnertype, "/", se.method,"/joint_optimal_avg/")
    create_directory_if_not_exist(jp_optimum_avg_path)
  }
}

# set_paths()

check_codes = function(){
  # checking the code
  # ---------------
  set_paths()
  
  self = ParallelLevel$new(benchmarks_name = benchmark_name, cluster = cluster, cores = 1)
  self = PredictionResults$new(benchmarks_name = benchmark_name, cluster = cluster, learnertype = learnertype, se.method = se.method)
  self$learnertype
  as = self$Single_Algorithm_Selection()
  mean(as$MCP)
  
  self = SequentialPerformance$new(benchmarks_name = benchmark_name, cluster = cluster)
  sbs = self$get_SBS()
  mean(self$get_mcp_dataframe(self)$glueminisat.2.2.5)
  vbs = self$get_VBS()
  mean(vbs$VBS_Runtime)
  
  self$benchmarks_name
  self$cores
  self$cores_str
  self$Cutoff
  self$actual_CSV_path
  self$features_path
  self$n_solvers
  self$n_instances
  self$solvers
  self$actual_CSV
  self$get_scenario()
  self$get_actual_result_csv()
  self$get_par10_dataframe()
  self$par10_CSV
  self$get_mcp_dataframe(sequentialData = SequentialPerformance$new(benchmarks_name = benchmark_name, cluster = cluster))
  self$mcp_CSV
  self$get_optimal_runtime()
  self$get_solved_instances()
  self$get_unsolved_instances()
  self$unsolvedInstances
  self$solvedInstances
  self$get_VBS()
  self$get_VBS_par10()
  self$get_VBS_mcp()
  self$get_solved_instances()
  self$get_unsolved_instances()
  self$get_SBS()
  self$get_solvers_solved_runtime(self$get_SBS())
  self$get_actual_result_csv()
  #
  self$get_mcp_dataframe(sequentialData = self)
  # self$ignore_instances()
  self$get_features()
  self$features
  self$get_nd_vbs_runtime(nd=2)
  self$ignore_instances()
  # self$get_nd_VBS(nd = 4)
  # nrow(self$get_nd_VBS(nd = 4))
  # split_dataframe_by_solver should be fixed
}

training = function(){
  #---------------
  #training 
  #---------------
  set_paths()
  self = SequentialPerformance$new(benchmarks_name = benchmark_name, cluster = cluster)
  self$train_aslibLike(savepath = modelPath, ignoreTimeouts = FALSE, train_by_par10 = FALSE, learnertype = learnertype, se.method = se.method)
  # self = PredictionResults$new(benchmark_name, cluster = cluster, learnertype = learnertype, se.method = se.method)
  # self$get_models_prediction()
}

# training()

training_km = function(){
  #---------------
  #training 
  #---------------
  library(mlr3)
  library(mlr3pipelines)
  library(paradox)
  impute_pipeline <- gunion(list(
          po("imputemean", id = "impute_numeric", param_vals = list(affect_columns = selector_type("numeric"))),
          po("imputemean", id = "impute_integer", param_vals = list(affect_columns = selector_type("integer"))),
          po("imputemode", id = "impute_logical", param_vals = list(affect_columns = selector_type("logical"))),
          po("imputeconstant", id = "impute_factor", param_vals = list(constant = "NA", affect_columns = selector_type("factor"))),
          po("imputeconstant", id = "impute_character", param_vals = list(constant = "NA", affect_columns = selector_type("character")))
        ))

  set_paths()
  self = SequentialPerformance$new(benchmarks_name = benchmark_name, cluster = cluster)
  self$train_aslibLike_km(savepath = modelPath, ignoreTimeouts = FALSE, train_by_par10 = FALSE, learnertype = learnertype, se.method = se.method, impute_pipeline= impute_pipeline)
  # self = PredictionResults$new(benchmark_name, cluster = cluster, learnertype = learnertype, se.method = se.method)
  # self$get_models_prediction()
}

# training_km()

get_top_vbs = function(saved = TRUE){
  print("get_top_vbs")
  set_paths()
  # # top vbs 
  if(!saved){
    self = SequentialPerformance$new(benchmarks_name = benchmark_name, cluster = cluster)
    vbs = self$get_VBS()
    range = c(1:max_core)
    top_vbs = data.frame(matrix(nrow = nrow(vbs), ncol = 0))
    top_vbs = cbind(top_vbs, vbs$InstanceName)
    for(i in range){
      par = ParallelLevel$new(benchmarks_name = benchmark_name, cluster = cluster, cores = i)
      top_n_vbs = vector()
      for(j in 1:i){
        nd_vbs = par$get_nd_vbs_runtime(instanceset = vbs$InstanceName, ignore_instances = FALSE, nd = j)
        top_n_vbs = cbind(top_n_vbs, nd_vbs$VBS_Runtime)
      }
      nd_vbs = rowMins(top_n_vbs)
      top_vbs = cbind(top_vbs,nd_vbs)
    }
    colnames(top_vbs) = c("InstanceName", str_c(rep("cores_",max_core), c(1:max_core)))
    write.csv(top_vbs, top_vbs_path, row.names = FALSE)
  } else{
    top_vbs = read.csv(top_vbs_path)
  }
  return(top_vbs)
}

# get_top_vbs(FALSE)

get_top_sbs = function(saved = TRUE){
  print("get_top_sbs")
  set_paths()
  if(!saved){
    self = SequentialPerformance$new(benchmarks_name = benchmark_name, cluster = cluster)
    vbs = self$get_VBS()
    range = c(1:max_core)
    top_sbs = data.frame(matrix(nrow = nrow(vbs), ncol = 0))
    top_sbs = cbind(top_sbs, vbs$InstanceName)
    for(i in range){
      solvers = colnames(self$actual_CSV[2:ncol(self$actual_CSV)])
      means = colMeans(self$actual_CSV[2:ncol(self$actual_CSV)])
      top_n_sbs = vector()
      for(j in 1:i){
        idx = which(means==min(means))
        top_n_sbs = append(top_n_sbs,solvers[idx])
        solvers = solvers[-idx]
        means = means[-idx]
      }
      top_n_sbs
      par = ParallelLevel$new(benchmarks_name = benchmark_name,cores= i, cluster = cluster)
      nd_sbs = par$get_actual_result_csv()[top_n_sbs]
      nd_sbs = rowMins(as.matrix(nd_sbs))
      print(par$get_actual_result_csv()$InstanceName == vbs$InstanceName)
      top_sbs = cbind(top_sbs,nd_sbs)
    }
    colnames(top_sbs) = c("InstanceName", str_c(rep("cores_",max_core), c(1:max_core)))
    write.csv(top_sbs, top_sbs_path, row.names = FALSE)
  } else{
    top_sbs = read.csv(top_sbs_path)
  }
  return(top_sbs)
}

# get_top_sbs(FALSE)

get_top_AS_noUncertainty_noSplitting = function(saved = TRUE){
  print("get_top_AS_noUncertainty_noSplitting")
  set_paths()
  #top AS, no uncertainty just top selected 
  #method number help:
  # -1 : running all in parallel, no selection just ordering based on prediction, (top_selection, orderBy doesn't work here)
  # 0 : algorithm selection, selection besed on predicted runtime, top_selection will choose top algorithms from data frame based on prediction
  # 1 : minpred <= pred <= [minpred + delta_prime*SE]
  # 2 : lowebound as good as min pred, [pred-delta*SE]<=minpred
  # 3 : generalized 2 and 1; p-delta*se <= minP + deltaPrime*SE_min
  # 4 : [minpred-delta_prime*SE]<=[pred-delta*SE]<=minpred
  self = PredictionResults$new(benchmark_name, cluster = cluster, learnertype = learnertype, se.method = se.method)
  range = c(1:max_core)
  resR = data.frame(matrix(ncol= 13,nrow=0))
  resMCP = data.frame(matrix(ncol= 13,nrow=0))
  resPar10 = data.frame(matrix(ncol= 13,nrow=0))
  resR_m = data.frame(matrix(ncol= 13,nrow=0))
  resMCP_m = data.frame(matrix(ncol= 13,nrow=0))
  resPar10_m = data.frame(matrix(ncol= 13,nrow=0))
  if(!saved){
    for(i in range){
      selectionPath = paste0(selectionPath_as_no_uncertainty,i,"_cores/")
      create_directory_if_not_exist(selectionPath)
      if(combined == "combined"){
        pred_path = paste0(predictionPath,"/preds_combined/")
      } else{ 
        pred_path = paste0(predictionPath,"/preds/")
      }
      self$selection_based_on_SE(predictionPath = pred_path, 
                                 saveTo = selectionPath, 
                                 method_number = 0, 
                                 top_selection = i, 
                                 ignoreTimeoutsOnVBS = FALSE, 
                                 orderBy = "pred")
      summary = self$get_summary_selection_result(ignoreTimeouts = FALSE,
                                                  selectionPath = selectionPath,
                                                  median = FALSE, 
                                                  method_number = 0
      )
      summary_med = self$get_summary_selection_result(ignoreTimeouts = FALSE,
                                                      selectionPath = selectionPath,
                                                      median = TRUE,
                                                      method_number = 0
      )
      resR = rbind(resR,summary[[1]])
      resMCP = rbind(resMCP,summary[[2]])
      resPar10 = rbind(resPar10,summary[[3]])
      resR_m = rbind(resR_m,summary_med[[1]])
      resMCP_m = rbind(resMCP_m,summary_med[[2]])
      resPar10_m = rbind(resPar10_m,summary_med[[3]])
    }
    summary = list(resR, resMCP, resPar10, resR_m, resMCP_m, resPar10_m)
    write.csv(resR,paste0(top_as_noUncertainty_path,"runtime_mean.csv"))
    write.csv(resMCP,paste0(top_as_noUncertainty_path,"mcp_mean.csv"))
    write.csv(resPar10,paste0(top_as_noUncertainty_path,"par10_mean.csv"))

    write.csv(resR_m,paste0(top_as_noUncertainty_path,"runtime_median.csv"))
    write.csv(resMCP_m,paste0(top_as_noUncertainty_path,"mcp_median.csv"))
    write.csv(resPar10_m,paste0(top_as_noUncertainty_path,"par10_median.csv"))
    
  } else{
    for(i in range){
      selectionPath = paste0(selectionPath_as_no_uncertainty,i,"_cores/")
      create_directory_if_not_exist(selectionPath)
      summary = self$get_summary_selection_result(ignoreTimeouts = FALSE,
                                                  selectionPath = selectionPath,
                                                  median = FALSE, 
                                                  method_number = 0
      )
      summary_med = self$get_summary_selection_result(ignoreTimeouts = FALSE,
                                                      selectionPath = selectionPath,
                                                      median = TRUE,
                                                      method_number = 0
      )
      resR = rbind(resR,summary[[1]])
      resMCP = rbind(resMCP,summary[[2]])
      resPar10 = rbind(resPar10,summary[[3]])
      resR_m = rbind(resR_m,summary_med[[1]])
      resMCP_m = rbind(resMCP_m,summary_med[[2]])
      resPar10_m = rbind(resPar10_m,summary_med[[3]])
    }
    summary = list(resR, resMCP, resPar10, resR_m, resMCP_m, resPar10_m)
  }
  return(summary)
}
 
# get_top_AS_noUncertainty_noSplitting(FALSE)

get_time_splitting_prediction = function(saved = TRUE){
  print("get_time_splitting_prediction")
  set_paths()
  # AS sequential time spliting 
  self = PredictionResults$new(benchmark_name, cluster = cluster, learnertype = learnertype, se.method = se.method)
  range = c(1:max_core)
  resR = data.frame(matrix(ncol= (max_core+1),nrow=0))
  resMCP = data.frame(matrix(ncol= (max_core+1),nrow=0))
  resPar10 = data.frame(matrix(ncol= (max_core+1),nrow=0))
  resR_m = data.frame(matrix(ncol= (max_core+1),nrow=0))
  resMCP_m = data.frame(matrix(ncol= (max_core+1),nrow=0))
  resPar10_m = data.frame(matrix(ncol= (max_core+1),nrow=0))
  if(!saved){
    for(c in range){
      path_to_schedule = paste0(timesplitting_schedule_path,c,"_cores/")
      create_directory_if_not_exist(path_to_schedule)
      if(combined == "combined"){
        pred_path = paste0(predictionPath,"/preds_combined/")
      } else{ 
        pred_path = paste0(predictionPath,"/preds/")
      }
      self$time_splitting_scheduling(predictionPath = pred_path, 
                                     selectionPath = path_to_schedule, 
                                     cores = c,
                                     ignoreTimeoutsOnVBS = FALSE, 
                                     orderBy = "pred")
      summary = self$time_splitting_scheduling_scraper(selectionPath = path_to_schedule,
                                                       ignoreTimeouts = FALSE,
                                                       median = FALSE,
                                                       orderBy = "pred")
      summary_med = self$time_splitting_scheduling_scraper(selectionPath = path_to_schedule,
                                                           ignoreTimeouts = FALSE,
                                                           median = TRUE,
                                                           orderBy = "pred")
      resR = rbind(resR, summary[[1]])
      resMCP = rbind(resMCP, summary[[2]])
      resPar10 = rbind(resPar10, summary[[3]])
      resR_m = rbind(resR_m,summary_med[[1]])
      resMCP_m = rbind(resMCP_m,summary_med[[2]])
      resPar10_m = rbind(resPar10_m,summary_med[[3]])
    }
    summary = list(resR, resMCP, resPar10, resR_m, resMCP_m, resPar10_m)
  } else { 
    for(c in range){
      path_to_schedule = paste0(timesplitting_schedule_path,c,"_cores/")
      create_directory_if_not_exist(path_to_schedule)
      summary = self$time_splitting_scheduling_scraper(selectionPath = path_to_schedule,
                                                       ignoreTimeouts = FALSE,
                                                       median = FALSE,
                                                       orderBy = "pred")
      summary_med = self$time_splitting_scheduling_scraper(selectionPath = path_to_schedule,
                                                           ignoreTimeouts = FALSE,
                                                           median = TRUE,
                                                           orderBy = "pred")
      resR = rbind(resR, summary[[1]])
      resMCP = rbind(resMCP, summary[[2]])
      resPar10 = rbind(resPar10, summary[[3]])
      resR_m = rbind(resR_m,summary_med[[1]])
      resMCP_m = rbind(resMCP_m,summary_med[[2]])
      resPar10_m = rbind(resPar10_m,summary_med[[3]])
    }
    summary = list(resR, resMCP, resPar10, resR_m, resMCP_m, resPar10_m)
  }
  return(summary)
}

# get_time_splitting_prediction(FALSE)

get_time_splitting_prediction_SE = function(saved = TRUE){

  print("get_time_splitting_prediction_SE")
  # AS sequential time spliting 
  set_paths()
  self = PredictionResults$new(benchmark_name, cluster = cluster, learnertype = learnertype, se.method = se.method)
  range = c(1:max_core)
  resR = data.frame(matrix(ncol= (max_core+1),nrow=0))
  resMCP = data.frame(matrix(ncol= (max_core+1),nrow=0))
  resPar10 = data.frame(matrix(ncol= (max_core+1),nrow=0))
  resR_m = data.frame(matrix(ncol= (max_core+1),nrow=0))
  resMCP_m = data.frame(matrix(ncol= (max_core+1),nrow=0))
  resPar10_m = data.frame(matrix(ncol= (max_core+1),nrow=0))
  if(!saved){
    for(c in range){
      path_to_schedule = paste0(timesplitting_se_schedule_path,c,"_cores/")
      create_directory_if_not_exist(path_to_schedule)
      if(combined == "combined"){
        pred_path = paste0(predictionPath,"/preds_combined/")
      } else{ 
        pred_path = paste0(predictionPath,"/preds/")
      }
      self$time_splitting_scheduling(predictionPath = pred_path, 
                                     selectionPath = path_to_schedule, 
                                     cores = c,
                                     ignoreTimeoutsOnVBS = FALSE, 
                                     orderBy = "pred+SE")
      summary = self$time_splitting_scheduling_scraper(selectionPath = path_to_schedule,
                                                       ignoreTimeouts = FALSE,
                                                       median = FALSE,
                                                       orderBy = "pred+SE")
      summary_med = self$time_splitting_scheduling_scraper(selectionPath = path_to_schedule,
                                                           ignoreTimeouts = FALSE,
                                                           median = TRUE,
                                                           orderBy = "pred+SE")
      resR = rbind(resR, summary[[1]])
      resMCP = rbind(resMCP, summary[[2]])
      resPar10 = rbind(resPar10, summary[[3]])
      resR_m = rbind(resR_m,summary_med[[1]])
      resMCP_m = rbind(resMCP_m,summary_med[[2]])
      resPar10_m = rbind(resPar10_m,summary_med[[3]])
    }
    summary = list(resR, resMCP, resPar10, resR_m, resMCP_m, resPar10_m)
  } else { 
    for(c in range){
      path_to_schedule = paste0(timesplitting_se_schedule_path,c,"_cores/")
      create_directory_if_not_exist(path_to_schedule)
      summary = self$time_splitting_scheduling_scraper(selectionPath = path_to_schedule,
                                                       ignoreTimeouts = FALSE,
                                                       median = FALSE,
                                                       orderBy = "pred+SE")
      summary_med = self$time_splitting_scheduling_scraper(selectionPath = path_to_schedule,
                                                           ignoreTimeouts = FALSE,
                                                           median = TRUE,
                                                           orderBy = "pred+SE")
      resR = rbind(resR, summary[[1]])
      resMCP = rbind(resMCP, summary[[2]])
      resPar10 = rbind(resPar10, summary[[3]])
      resR_m = rbind(resR_m,summary_med[[1]])
      resMCP_m = rbind(resMCP_m,summary_med[[2]])
      resPar10_m = rbind(resPar10_m,summary_med[[3]])
    }
    summary = list(resR, resMCP, resPar10, resR_m, resMCP_m, resPar10_m)
  }
  return(summary)
}

# get_time_splitting_prediction_SE(FALSE)

get_flexfolio_3s_results = function(){
  print("get_flexfolio_3s_results")
  set_paths()
  self = PredictionResults$new(benchmark_name, cluster = cluster, learnertype = learnertype, se.method = se.method)
  range = c(1:max_core)
  resR = data.frame(matrix(ncol= 6,nrow=0))
  resR_m = data.frame(matrix(ncol= 6,nrow=0))
  
  for(c in range){
    path_to_results = paste0(flexfolio_3s_path,
                            c,"core-3s.csv")
    csv = read.csv(path_to_results)
    csv$par10 = csv$claspfolio
    csv$claspfolio[which(csv$claspfolio == self$sequentialData$Cutoff * 10)] <- self$sequentialData$Cutoff
    csv= csv[order(csv$Instance),]
    vbs = get_top_vbs()[1:2]
    csv$vbs = vbs$cores_1
    csv$mcp = csv$claspfolio - csv$vbs
    
    resR = rbind(resR, c(mean(csv$vbs), mean(csv$claspfolio), mean(csv$mcp), mean(csv$par10),c, "FALSE"))
    colnames(resR) <- c("vbs", "runtime", "mcp", "par10","cores", "median")    
    resR_m = rbind(resR_m, c(median(csv$vbs), median(csv$claspfolio), median(csv$mcp), median(csv$par10),c, "TRUE"))
    colnames(resR_m) <- c("vbs", "runtime", "mcp", "par10","cores", "median")
  }
  summary = list(resR, resR_m)
  return(summary)
}

# get_flexfolio_3s_results()

get_joint_probability_optimum = function(){
  print("get_joint_probability_optimum")
  set_paths()
  self = PredictionResults$new(benchmark_name, cluster = cluster, learnertype = learnertype, se.method = se.method)
  range = seq(0,1,by=0.01)
  min = Inf
  optimum = Inf
  range = range[1:101]
  for(r in range){
    create_directory_if_not_exist(paste0(joint_probability_path,"pcap_",r))
    if(combined == "combined"){
      pred_path = paste0(predictionPath,"/preds_combined/")
    } else{ 
      pred_path = paste0(predictionPath,"/preds/")
    }
    self$selection_based_on_SE(predictionPath = pred_path, 
                               saveTo = paste0(joint_probability_path,"pcap_",r),
                               method_number = 5,
                               top_selection = max_core,
                               ignoreTimeoutsOnVBS = FALSE,
                               orderBy = "pred",
                               delta = 0,
                               delta_prime = 0,
                               alpha = 0,
                               JP_limit = r)
    summary = self$get_summary_selection_result(paste0(joint_probability_path,"pcap_",r),
                                                 ignoreTimeouts = FALSE,
                                                 method_number = 5,
                                                 median = FALSE)
    print(r)
    print(summary)
    if(as.numeric(summary[[1]]$Parallel_time)<=min) {
      min = as.numeric(summary[[1]]$Parallel_time)
      optimum = r
    }
  }
  # unlink(joint_probability_path,recursive=TRUE)
  return(optimum)
}

# jp_optimum = get_joint_probability_optimum()
# write(paste0(Sys.Date(), " : ", benchmark_name, " - ", learnertype, " - ", se.method, " - ", jp_optimum), file = paste0(basepath,"jp_optimum_result.txt"), append = TRUE)

get_joint_probability_results = function(saved = TRUE, optimum = jp_optimum){
  print("get_joint_probability_results")
  set_paths()
  self = PredictionResults$new(benchmark_name, cluster = cluster, learnertype = learnertype, se.method = se.method)
  range = c(1:max_core)
  resR = data.frame(matrix(ncol= (max_core+1),nrow=0))
  resMCP = data.frame(matrix(ncol= (max_core+1),nrow=0))
  resPar10 = data.frame(matrix(ncol= (max_core+1),nrow=0))
  resR_m = data.frame(matrix(ncol= (max_core+1),nrow=0))
  resMCP_m = data.frame(matrix(ncol= (max_core+1),nrow=0))
  resPar10_m = data.frame(matrix(ncol= (max_core+1),nrow=0))
  if(!saved){
    for(i in range){
      saveto= paste0(jp_optimum_path,i, "-core")
      create_directory_if_not_exist(saveto)
      if(combined == "combined"){
        pred_path = paste0(predictionPath,"/preds_combined/")
      } else{ 
        pred_path = paste0(predictionPath,"/preds/")
      }
      self$selection_based_on_SE(predictionPath = pred_path, 
                                 saveTo = saveto,
                                 method_number = 5,
                                 top_selection = i,
                                 ignoreTimeoutsOnVBS = FALSE,
                                 orderBy = "pred",
                                 delta = 0,
                                 delta_prime = 0,
                                 alpha = 0,
                                 JP_limit = optimum)
      summary = self$get_summary_selection_result(saveto,
                                                  ignoreTimeouts = FALSE,
                                                  method_number = 5,
                                                  median = FALSE)
      summary_med = self$get_summary_selection_result(saveto,
                                                      ignoreTimeouts = FALSE,
                                                      method_number = 5,
                                                      median = TRUE)
      resR = rbind(resR, summary[[1]])
      resMCP = rbind(resMCP, summary[[2]])
      resPar10 = rbind(resPar10, summary[[3]])
      resR_m = rbind(resR_m,summary_med[[1]])
      resMCP_m = rbind(resMCP_m,summary_med[[2]])
      resPar10_m = rbind(resPar10_m,summary_med[[3]])
    }
  } else{ 
    for(i in range){
      saveto= paste0(jp_optimum_path,i, "-core")
      create_directory_if_not_exist(saveto)
      summary = self$get_summary_selection_result(saveto,
                                                  ignoreTimeouts = FALSE,
                                                  method_number = 5,
                                                  median = FALSE)
      summary_med = self$get_summary_selection_result(saveto,
                                                      ignoreTimeouts = FALSE,
                                                      method_number = 5,
                                                      median = TRUE)
      resR = rbind(resR, summary[[1]])
      resMCP = rbind(resMCP, summary[[2]])
      resPar10 = rbind(resPar10, summary[[3]])
      resR_m = rbind(resR_m,summary_med[[1]])
      resMCP_m = rbind(resMCP_m,summary_med[[2]])
      resPar10_m = rbind(resPar10_m,summary_med[[3]])
    }
  }
  summary = list(resR, resMCP, resPar10, resR_m, resMCP_m, resPar10_m)
  return(summary)
}

# get_joint_probability_results(saved = FALSE, optimum = jp_optimum)

get_kl_div_optimum = function(){
  print("get_kl_div_optimum")
  set_paths()
  self = PredictionResults$new(benchmark_name, cluster = cluster, learnertype = learnertype, se.method = se.method)
  # range = seq(0,1,by=0.01)
  range = seq(0,3,by=0.01)
  min = Inf
  optimum = Inf
  range = range[1:301]
  for(r in range){
    create_directory_if_not_exist(paste0(kl_div_path,"kl_div_",r))
    if(combined == "combined"){
      pred_path = paste0(predictionPath,"/preds_combined/")
    } else{ 
      pred_path = paste0(predictionPath,"/preds/")
    }
    self$selection_based_on_SE(predictionPath = pred_path, 
                               saveTo = paste0(kl_div_path,"kl_div_",r),
                               method_number = 6,
                               top_selection = max_core,
                               ignoreTimeoutsOnVBS = FALSE,
                               orderBy = "pred",
                               delta = 0,
                               delta_prime = 0,
                               alpha = 0,
                               JP_limit = 0.1, 
                               KL_limit = r)
    summary = self$get_summary_selection_result(paste0(kl_div_path,"kl_div_",r),
                                                 ignoreTimeouts = FALSE,
                                                 method_number = 6,
                                                 median = FALSE)
    print(r)
    print(summary)
    if(as.numeric(summary[[1]]$Parallel_time)<=min) {
      min = as.numeric(summary[[1]]$Parallel_time)
      optimum = r
    }
  }
  # unlink(kl_div_path,recursive=TRUE)
  return(optimum)
}

kl_optimum = get_kl_div_optimum()
write(paste0(Sys.Date(), " : ", benchmark_name, " - ", learnertype, " - ", se.method, " - ", kl_optimum), file = paste0(basepath,"kl_optimum_result.txt"), append = TRUE)

get_kl_div_results = function(saved = TRUE, optimum = kl_optimum){
  print("get_joint_probability_results")
  set_paths()
  self = PredictionResults$new(benchmark_name, cluster = cluster, learnertype = learnertype, se.method = se.method)
  range = c(1:max_core)
  resR = data.frame(matrix(ncol= (max_core+1),nrow=0))
  resMCP = data.frame(matrix(ncol= (max_core+1),nrow=0))
  resPar10 = data.frame(matrix(ncol= (max_core+1),nrow=0))
  resR_m = data.frame(matrix(ncol= (max_core+1),nrow=0))
  resMCP_m = data.frame(matrix(ncol= (max_core+1),nrow=0))
  resPar10_m = data.frame(matrix(ncol= (max_core+1),nrow=0))
  if(!saved){
    for(i in range){
      saveto= paste0(kl_optimum_path,i, "-core")
      create_directory_if_not_exist(saveto)
      if(combined == "combined"){
        pred_path = paste0(predictionPath,"/preds_combined/")
      } else{ 
        pred_path = paste0(predictionPath,"/preds/")
      }
      self$selection_based_on_SE(predictionPath = pred_path, 
                                 saveTo = saveto,
                                 method_number = 6,
                                 top_selection = i,
                                 ignoreTimeoutsOnVBS = FALSE,
                                 orderBy = "pred",
                                 delta = 0,
                                 delta_prime = 0,
                                 alpha = 0,
                                 JP_limit = 0.1,
                                 KL_limit = optimum)
      summary = self$get_summary_selection_result(saveto,
                                                  ignoreTimeouts = FALSE,
                                                  method_number = 6,
                                                  median = FALSE)
      summary_med = self$get_summary_selection_result(saveto,
                                                      ignoreTimeouts = FALSE,
                                                      method_number = 6,
                                                      median = TRUE)
      resR = rbind(resR, summary[[1]])
      resMCP = rbind(resMCP, summary[[2]])
      resPar10 = rbind(resPar10, summary[[3]])
      resR_m = rbind(resR_m,summary_med[[1]])
      resMCP_m = rbind(resMCP_m,summary_med[[2]])
      resPar10_m = rbind(resPar10_m,summary_med[[3]])
    }
  } else{ 
    for(i in range){
      saveto= paste0(kl_optimum_path,i, "-core")
      create_directory_if_not_exist(saveto)
      summary = self$get_summary_selection_result(saveto,
                                                  ignoreTimeouts = FALSE,
                                                  method_number = 6,
                                                  median = FALSE)
      summary_med = self$get_summary_selection_result(saveto,
                                                      ignoreTimeouts = FALSE,
                                                      method_number = 6,
                                                      median = TRUE)
      resR = rbind(resR, summary[[1]])
      resMCP = rbind(resMCP, summary[[2]])
      resPar10 = rbind(resPar10, summary[[3]])
      resR_m = rbind(resR_m,summary_med[[1]])
      resMCP_m = rbind(resMCP_m,summary_med[[2]])
      resPar10_m = rbind(resPar10_m,summary_med[[3]])
    }
  }
  summary = list(resR, resMCP, resPar10, resR_m, resMCP_m, resPar10_m)
  return(summary)
}
# 
print(kl_optimum)
get_kl_div_results(saved = FALSE, optimum = kl_optimum)

get_joint_probability_results_averageOpt = function(saved = TRUE, optimum = jp_average_optimum){
  print("get_joint_probability_results_averageOpt")
  set_paths()
  self = PredictionResults$new(benchmark_name, cluster = cluster, learnertype = learnertype, se.method = se.method)
  range = c(1:max_core)
  resR = data.frame(matrix(ncol= (max_core+1),nrow=0))
  resMCP = data.frame(matrix(ncol= (max_core+1),nrow=0))
  resPar10 = data.frame(matrix(ncol= (max_core+1),nrow=0))
  resR_m = data.frame(matrix(ncol= (max_core+1),nrow=0))
  resMCP_m = data.frame(matrix(ncol= (max_core+1),nrow=0))
  resPar10_m = data.frame(matrix(ncol= (max_core+1),nrow=0))
  if(!saved){
    for(i in range){
      saveto = paste0(jp_optimum_avg_path,i, "-core")
      create_directory_if_not_exist(saveto)
      if(combined == "combined"){
        pred_path = paste0(predictionPath,"/preds_combined/")
      } else{ 
        pred_path = paste0(predictionPath,"/preds/")
      }
      self$selection_based_on_SE(predictionPath = pred_path, 
                                 saveTo = saveto, 
                                 method_number = 5,
                                 top_selection = i,
                                 ignoreTimeoutsOnVBS = FALSE,
                                 orderBy = "pred",
                                 delta = 0,
                                 delta_prime = 0,
                                 alpha = 0,
                                 JP_limit = optimum)
      summary = self$get_summary_selection_result(saveto,
                                                  ignoreTimeouts = FALSE,
                                                  method_number = 5,
                                                  median = FALSE)
      summary_med = self$get_summary_selection_result(saveto,
                                                      ignoreTimeouts = FALSE,
                                                      method_number = 5,
                                                      median = TRUE)
      resR = rbind(resR, summary[[1]])
      resMCP = rbind(resMCP, summary[[2]])
      resPar10 = rbind(resPar10, summary[[3]])
      resR_m = rbind(resR_m,summary_med[[1]])
      resMCP_m = rbind(resMCP_m,summary_med[[2]])
      resPar10_m = rbind(resPar10_m,summary_med[[3]])
    }
  } else{ 
    for(i in range){
      saveto = paste0(jp_optimum_avg_path,i, "-core")
      create_directory_if_not_exist(saveto)
      summary = self$get_summary_selection_result(saveto,
                                                  ignoreTimeouts = FALSE,
                                                  method_number = 5,
                                                  median = FALSE)
      summary_med = self$get_summary_selection_result(saveto,
                                                      ignoreTimeouts = FALSE,
                                                      method_number = 5,
                                                      median = TRUE)
      resR = rbind(resR, summary[[1]])
      resMCP = rbind(resMCP, summary[[2]])
      resPar10 = rbind(resPar10, summary[[3]])
      resR_m = rbind(resR_m,summary_med[[1]])
      resMCP_m = rbind(resMCP_m,summary_med[[2]])
      resPar10_m = rbind(resPar10_m,summary_med[[3]])
    }
  }
  summary = list(resR, resMCP, resPar10, resR_m, resMCP_m, resPar10_m)
  return(summary)
}

# get_joint_probability_results_averageOpt(FALSE, optimum = jp_average_optimum)

scrape_all_results = function(){
  self = PredictionResults$new(benchmarks_name = benchmark_name, cluster = cluster, learnertype = learnertype, se.method = se.method)
  print("scrape_all_results")
  set_paths()
  df_results = data.frame(matrix(nrow = 0, ncol = 9))
  
  top_vbs = get_top_vbs()
  for(c in colnames(top_vbs)[2:ncol(top_vbs)]){
    top_vbs[c][[1]] <- as.numeric(unlist(top_vbs[c][[1]]))
    if(c == "cores_1"){
      par10 = top_vbs$cores_1
      par10[which(par10== self$sequentialData$Cutoff)] <- self$sequentialData$Cutoff * 10
      row = c("VBS", 1, mean(top_vbs$cores_1), 0, mean(par10), median(top_vbs$cores_1), 0, median(par10),"Sequential VBS, Oracle")
      df_results= rbind(df_results,row)
      colnames(df_results) = c("Approach", "cores", "Runtime", "MCP", "PAR10", "median_Runtime", "median_MCP", "median_PAR10","decription")
    } else{
      runtime = top_vbs[c][[1]]
      par10 = runtime
      par10[which(par10== self$sequentialData$Cutoff)] = self$sequentialData$Cutoff * 10
      mcp = runtime - top_vbs$cores_1
      row = c("VBS", str_split(c,"cores_")[[1]][2], 
              mean(runtime), mean(mcp), mean(par10),
              median(runtime), median(mcp), median(par10),
              paste0("Parallel VBS,",c))
      df_results= rbind(df_results,row)
    }
  }
  
  top_sbs = get_top_sbs()
  for(c in colnames(top_sbs)[2:ncol(top_sbs)]){
    top_sbs[c][[1]] <- as.numeric(unlist(top_sbs[c][[1]]))
    if(c == "cores_1"){
      
      runtime = top_sbs[c][[1]]
      par10 = top_sbs$cores_1
      par10[which(par10== self$sequentialData$Cutoff)] <- self$sequentialData$Cutoff * 10
      mcp = runtime - top_vbs$cores_1
      row = c("SBS", 1, mean(top_sbs$cores_1), mean(mcp), mean(par10), median(top_sbs$cores_1), median(mcp),median(par10),"Sequential SBS")
      df_results= rbind(df_results,row)
      colnames(df_results) = c("Approach", "cores", "Runtime", "MCP", "PAR10", "median_Runtime", "median_MCP", "median_PAR10","decription")
    } else{
      runtime = top_sbs[c][[1]]
      par10 = runtime
      par10[which(par10== self$sequentialData$Cutoff)] = self$sequentialData$Cutoff * 10
      mcp = runtime - top_vbs$cores_1
      row = c("SBS", str_split(c,"cores_")[[1]][2], 
              mean(runtime), mean(mcp), mean(par10), 
              median(runtime), median(mcp), median(par10), 
              paste0("Parallel SBS,",c))
      df_results= rbind(df_results,row)
    }
  }
  
  top_as = get_top_AS_noUncertainty_noSplitting()
  for(c in 1:nrow(top_as[[1]])){
    if(c == 1){
      row = c("AS", 1, top_as[[1]][1,]$Parallel_time, top_as[[2]][1,]$Parallel_time, top_as[[3]][1,]$Parallel_time, top_as[[4]][1,]$Parallel_time, top_as[[5]][1,]$Parallel_time, top_as[[6]][1,]$Parallel_time,"Single Algorithm Selection")
      df_results= rbind(df_results,row)
    } else{ 
      row = c("AS", c, top_as[[1]][c,]$Parallel_time, top_as[[2]][c,]$Parallel_time, top_as[[3]][c,]$Parallel_time,top_as[[4]][c,]$Parallel_time, top_as[[5]][c,]$Parallel_time, top_as[[6]][c,]$Parallel_time, "Algorithm Selection - Top, based on prediction, no uncertainty or time splitting")
      df_results= rbind(df_results,row)
    }
  }
  
  # time_splitting_pred = get_time_splitting_prediction()
  # for(c in 1:nrow(time_splitting_pred[[1]])){
  #   if(c == 1){
  #     row = c("Time Splitting - Prediction", 1, time_splitting_pred[[1]][1,]$Schedule_time, time_splitting_pred[[2]][1,]$Schedule_time, time_splitting_pred[[3]][1,]$Schedule_time,
  #             time_splitting_pred[[4]][1,]$Schedule_time, time_splitting_pred[[5]][1,]$Schedule_time, time_splitting_pred[[6]][1,]$Schedule_time, "Sequential Timesplitting based on Predictions")
  #     df_results= rbind(df_results,row)
  #   } else{
  #     row = c("Time Splitting - Prediction", c, time_splitting_pred[[1]][c,]$Schedule_time, time_splitting_pred[[2]][c,]$Schedule_time, time_splitting_pred[[3]][c,]$Schedule_time,
  #             time_splitting_pred[[4]][c,]$Schedule_time, time_splitting_pred[[5]][c,]$Schedule_time, time_splitting_pred[[6]][c,]$Schedule_time, "Parallel Timesplitting based on Predictions - Grid bin packing")
  #     df_results= rbind(df_results,row)
  #   }
  # }
  time_splitting_predSE = get_time_splitting_prediction_SE()
  for(c in 1:nrow(time_splitting_predSE[[1]])){
    if(c == 1){
      row = c("Time Splitting - Prediction+SE", 1, time_splitting_predSE[[1]][1,]$Schedule_time, time_splitting_predSE[[2]][1,]$Schedule_time, time_splitting_predSE[[3]][1,]$Schedule_time,
              time_splitting_predSE[[4]][1,]$Schedule_time, time_splitting_predSE[[5]][1,]$Schedule_time, time_splitting_predSE[[6]][1,]$Schedule_time,"Sequential Timesplitting based on Predictions+SE")
      df_results= rbind(df_results,row)
    } else{
      row = c("Time Splitting - Prediction+SE", c, time_splitting_predSE[[1]][c,]$Schedule_time, time_splitting_predSE[[2]][c,]$Schedule_time, time_splitting_predSE[[3]][c,]$Schedule_time,
              time_splitting_predSE[[4]][c,]$Schedule_time, time_splitting_predSE[[5]][c,]$Schedule_time, time_splitting_predSE[[6]][c,]$Schedule_time, "Parallel Timesplitting based on Predictions+SE - Grid bin packing")
      df_results= rbind(df_results,row)
    }
  }
  
  # time_splitting_predSE = get_time_splitting_prediction_aSE(a = 2)
  # for(c in 1:nrow(time_splitting_predSE[[1]])){
  #   if(c == 1){
  #     row = c("Time Splitting - Prediction+2SE", 1, time_splitting_predSE[[1]][1,]$Schedule_time, time_splitting_predSE[[2]][1,]$Schedule_time, time_splitting_predSE[[3]][1,]$Schedule_time,
  #             time_splitting_predSE[[4]][1,]$Schedule_time, time_splitting_predSE[[5]][1,]$Schedule_time, time_splitting_predSE[[6]][1,]$Schedule_time,"Sequential Timesplitting based on Predictions+SE")
  #     df_results= rbind(df_results,row)
  #   } else{
  #     row = c("Time Splitting - Prediction+2SE", c, time_splitting_predSE[[1]][c,]$Schedule_time, time_splitting_predSE[[2]][c,]$Schedule_time, time_splitting_predSE[[3]][c,]$Schedule_time,
  #             time_splitting_predSE[[4]][c,]$Schedule_time, time_splitting_predSE[[5]][c,]$Schedule_time, time_splitting_predSE[[6]][c,]$Schedule_time, "Parallel Timesplitting based on Predictions+SE - Grid bin packing")
  #     df_results= rbind(df_results,row)
  #   }
  # }
  # optimum = get_joint_probability_optimum()
  optimum = jp_optimum
  JoinProbabilityResults = get_joint_probability_results(saved = TRUE, optimum)
  for(c in 1:nrow(JoinProbabilityResults[[1]])){
    if(c == 1){
      row = c("Uncertainty - JointProbability", 1, JoinProbabilityResults[[1]][1,]$Parallel_time, JoinProbabilityResults[[2]][1,]$Parallel_time, JoinProbabilityResults[[3]][1,]$Parallel_time, 
              JoinProbabilityResults[[4]][1,]$Parallel_time, JoinProbabilityResults[[5]][1,]$Parallel_time, JoinProbabilityResults[[6]][1,]$Parallel_time,paste0("Uncertainty - JointProbability - optimum = ",optimum))
      df_results= rbind(df_results,row)
    } else{ 
      row = c("Uncertainty - JointProbability", c, JoinProbabilityResults[[1]][c,]$Parallel_time, JoinProbabilityResults[[2]][c,]$Parallel_time, JoinProbabilityResults[[3]][c,]$Parallel_time, 
              JoinProbabilityResults[[4]][c,]$Parallel_time, JoinProbabilityResults[[5]][c,]$Parallel_time, JoinProbabilityResults[[6]][c,]$Parallel_time, paste0("Uncertainty - JointProbability - optimum = ",optimum))
      df_results= rbind(df_results,row)
    }
  }
  
  kl_optimum = kl_optimum
  KLDivResults = get_kl_div_results(saved = TRUE, kl_optimum)
  for(c in 1:nrow(KLDivResults[[1]])){
    if(c == 1){
      row = c("Uncertainty - KL Div", 1, KLDivResults[[1]][1,]$Parallel_time, KLDivResults[[2]][1,]$Parallel_time, KLDivResults[[3]][1,]$Parallel_time, 
              KLDivResults[[4]][1,]$Parallel_time, KLDivResults[[5]][1,]$Parallel_time, KLDivResults[[6]][1,]$Parallel_time,paste0("Uncertainty - KL Div - optimum = ",kl_optimum))
      df_results= rbind(df_results,row)
    } else{ 
      row = c("Uncertainty - KL Div", c, KLDivResults[[1]][c,]$Parallel_time, KLDivResults[[2]][c,]$Parallel_time, KLDivResults[[3]][c,]$Parallel_time, 
              KLDivResults[[4]][c,]$Parallel_time, KLDivResults[[5]][c,]$Parallel_time, KLDivResults[[6]][c,]$Parallel_time, paste0("Uncertainty - KL Div - optimum = ",kl_optimum))
      df_results= rbind(df_results,row)
    }
  }
  
  flexfolio_3s = get_flexfolio_3s_results()
  for(c in 1:nrow(flexfolio_3s[[1]])){
    if(c == 1){
      row = c("3S", 1, flexfolio_3s[[1]][1,]$runtime, flexfolio_3s[[1]][1,]$mcp, flexfolio_3s[[1]][1,]$par10, 
              flexfolio_3s[[2]][1,]$runtime, flexfolio_3s[[2]][1,]$mcp, flexfolio_3s[[2]][1,]$par10,"Flexfolio implemantation of 3S, using sequential scenarios")
      df_results= rbind(df_results,row)
    } else{ 
      row = c("3S", c, flexfolio_3s[[1]][c,]$runtime, flexfolio_3s[[1]][c,]$mcp, flexfolio_3s[[1]][c,]$par10, 
              flexfolio_3s[[2]][c,]$runtime, flexfolio_3s[[2]][c,]$mcp, flexfolio_3s[[2]][c,]$par10,"Flexfolio implemantation of 3S, using parallel scenarios")
      df_results= rbind(df_results,row)
    }
  }
  return(df_results)
}

# scrape_all_results()

plot_vbs_stats = function(save=FALSE){
  print("plot_vbs_stats")
  set_paths()
  top_vbs = get_top_vbs()
  top_vbs[2:(max_core+1)] = apply(top_vbs[2:(max_core+1)],2,as.numeric)
  
  sd = apply(top_vbs[2:(max_core+1)],2,sd)
  mean = apply(top_vbs[2:(max_core+1)],2,mean)
  median = apply(top_vbs[2:(max_core+1)],2,median)
  sum_top_vbs = data.frame(mean = mean, median = median, sd = sd)
  sum_top_vbs$cores = c(1:max_core)
  sum_top_vbs = sum_top_vbs[which(!(sum_top_vbs$cores%in% c((max_core+1):19,20:29,31, 33:38))),]
  
  p<-ggplot(sum_top_vbs, aes(x=cores, y=mean,colour="Mean VBS Runtime")) +
    geom_point()+
    geom_point(aes(y=median,colour="Median VBS Runtime"))+
    geom_errorbar(aes(ymin=mean-sd,
                      ymax=mean+sd), width=.2,
                  position=position_dodge(0.05))+
    scale_y_continuous(
      name = "Runtime"
      #sec.axis = sec_axis( trans=~./self$Cutoff , name="probability of solving instance RegRF prediction")
    )
  if(save){
    ggsave(dpi = 500, width = 7, height = 5, filename = paste0(basepath,"VBS_behavior_different_cores_runtime.pdf"))
  }
  return(p)
}

# plot_vbs_stats(TRUE)

plot_sbs_stats = function(save=FALSE){
  print("plot_sbs_stats")
  set_paths()
  top_sbs = get_top_sbs()
  top_sbs[2:(max_core+1)] = apply(top_sbs[2:(max_core+1)],2,as.numeric)
  
  sd = apply(top_sbs[2:(max_core+1)],2,sd)
  mean = apply(top_sbs[2:(max_core+1)],2,mean)
  median = apply(top_sbs[2:(max_core+1)],2,median)
  sum_top_sbs = data.frame(mean = mean, median = median, sd = sd)
  sum_top_sbs$cores = c(1:max_core)
  sum_top_sbs = sum_top_sbs[which(!(sum_top_sbs$cores%in% c((max_core+1):19,20:29,31, 33:38))),]
  
  p<-ggplot(sum_top_sbs, aes(x=cores, y=mean,colour="Mean SBS Runtime")) +
    geom_point()+
    geom_point(aes(y=median,colour="Median SBS Runtime"))+
    geom_errorbar(aes(ymin=mean-sd,
                      ymax=mean+sd), width=.2,
                  position=position_dodge(0.05))
    # scale_y_continuous(
      # name = "Runtime",
      #sec.axis = sec_axis( trans=~./
      # Cutoff , name="probability of solving instance RegRF prediction")
    
  if(save){
    ggsave(filename = paste0(basepath,"SBS_behavior_different_cores_runtime.pdf"),dpi = 500, width = 7, height = 5)
  }
  return(p)
}

# plot_sbs_stats(TRUE)

plot_all_results = function(save = FALSE, metric = "Runtime"){
  print("plot_all_results")
  set_paths()
  results = scrape_all_results()
  results[2:8] <- lapply(results[2:8],as.numeric)
  optimum = jp_optimum
  if(metric == "Runtime"){
    p <- ggplot(results, aes(x=cores, y=Runtime, colour = Approach, shape = Approach)) +
      geom_point(size = 3)+
      geom_line(size = 1)+
      # geom_line(aes(linetype="Mean"), size = 1)+
      # geom_point(aes(y=Median),size = 3)+
      # geom_line(aes(y = Median, linetype = "Median"), size = 1)+
      guides(linetype=guide_legend(title="",keywidth = 3, keyheight = 1.5),
             colour = guide_legend(keywidth = 3, keyheight = 1.5))+
      expand_limits(x = 1)+ theme(text=element_text(size=22,  family="Times"))+
      labs(x = "Cores", y = "Runtime (s)", title = paste0(plot_title,"_",learnertype,"_",se.method))+theme(plot.title = element_text(hjust = 0.5))+
      scale_x_continuous(breaks=c(1:max_core))+
      scale_color_discrete(labels=c("3S",bquote(AS[0]), 'SBS',"Time Splitting",
                                    bquote(AS[~p[~'\u2229']]), bquote(AS[KL]), 'VBS'))+
      scale_shape_discrete(labels=c("3S",bquote(AS[0]), 'SBS',"Time Splitting",
                                    bquote(AS[~p[~'\u2229']]), bquote(AS[KL]), 'VBS'))
      # scale_y_continuous(trans='log10')
    if(save){
      ggsave(dpi = 500, width = 9, height = 5, filename = paste0(basepath,benchmark_name,"_line_chart_parallel_runtime.pdf"))
    }
  } else if(metric == "MCP"){
    p <- ggplot(results, aes(x=cores, y=MCP, colour = Approach, shape = Approach)) +
      geom_point(size = 3)+
      geom_line(size = 1)+
      # geom_line(aes(linetype="Mean"), size = 1)+
      # geom_point(aes(y=Median),size = 3)+
      # geom_line(aes(y = Median, linetype = "Median"), size = 1)+
      guides(linetype=guide_legend(title="",keywidth = 3, keyheight = 1.5),
             colour = guide_legend(keywidth = 3, keyheight = 1.5))+
      expand_limits(x = 1)+ theme(text=element_text(size=22,  family="Times"))+
      labs(x = "Cores", y = "MCP", title = paste0(plot_title,"_",learnertype,"_",se.method))+theme(plot.title = element_text(hjust = 0.5))+
      scale_x_continuous(breaks=c(1:max_core))+
      scale_color_discrete(labels=c("3S",bquote(AS[0]), 'SBS',"Time Splitting",
                                    bquote(AS[~p[~'\u2229']]), bquote(AS[KL]), 'VBS'))+
      scale_shape_discrete(labels=c("3S",bquote(AS[0]), 'SBS',"Time Splitting",
                                    bquote(AS[~p[~'\u2229']]), bquote(AS[KL]), 'VBS'))
      
      # scale_y_continuous(trans='log10')
    if(save){
      ggsave(dpi = 500, width = 9, height = 5, filename = paste0(basepath,benchmark_name,"_line_chart_parallel_MCP.pdf"))
    }
  } else if(metric == "PAR10"){
    p <- ggplot(results, aes(x=cores, y=PAR10, colour = Approach, shape = Approach)) +
      geom_point(size = 3)+
      geom_line(size = 1)+
      # geom_line(aes(linetype="Mean"), size = 1)+
      # geom_point(aes(y=Median),size = 3)+
      # geom_line(aes(y = Median, linetype = "Median"), size = 1)+
      guides(linetype=guide_legend(title="",keywidth = 3, keyheight = 1.5),
             colour = guide_legend(keywidth = 3, keyheight = 1.5))+
      expand_limits(x = 1)+ theme(text=element_text(size=22,  family="Times"))+
      labs(x = "Cores", y = "PAR10", title = paste0(plot_title,"_",learnertype,"_",se.method))+theme(plot.title = element_text(hjust = 0.5))+
      scale_x_continuous(breaks=c(1:max_core))+
      scale_color_discrete(labels=c("3S",bquote(AS[0]), 'SBS',"Time Splitting",
                                    bquote(AS[~p[~'\u2229']]), bquote(AS[KL]), 'VBS'))+
      scale_shape_discrete(labels=c("3S",bquote(AS[0]), 'SBS',"Time Splitting",
                                    bquote(AS[~p[~'\u2229']]), bquote(AS[KL]), 'VBS'))
      # scale_y_continuous(trans='log10')
    if(save){
      ggsave(dpi = 500, width = 9, height = 5, filename = paste0(basepath,benchmark_name,"_line_chart_parallel_PAR10.pdf"))
    }
  }
  return(p)
}

# plot_all_results(TRUE)

plot_all_results_normalized_gap = function(save = FALSE){
  print("plot_all_results_normalized_gap")
  set_paths()
  #vbs is 1
  #sbs is 0
  #sbs-value/sbs-vbs
  results = scrape_all_results()
  results[2:8] <- lapply(results[2:8],as.numeric)
  results = results[c(1,2,5,8)]
  self = PredictionResults$new(benchmarks_name = benchmark_name, cluster = cluster, learnertype = learnertype, se.method = se.method)
  par10_sbs = get_top_sbs()$cores_1
  par10_sbs[which(par10_sbs== self$sequentialData$Cutoff)] <- self$sequentialData$Cutoff * 10
  par10_sbs = mean(par10_sbs)
  par10_vbs = get_top_vbs()$cores_1
  par10_vbs[which(par10_vbs== self$sequentialData$Cutoff)] <- self$sequentialData$Cutoff * 10
  par10_vbs = mean(par10_vbs)
  results$PAR10= (par10_sbs - results$PAR10)/(par10_sbs - par10_vbs)
  results$PAR10 = as.numeric(results$PAR10)
  
  p = ggplot(results, aes(x=cores, y=PAR10, colour = Approach, shape = Approach)) +
    geom_point(size = 3)+
    geom_line(size = 1)+
    # geom_line(aes(linetype="Mean"), size = 1)+
    # geom_point(aes(y=Median),size = 3)+
    # geom_line(aes(y = Median, linetype = "Median"), size = 1)+
    guides(linetype=guide_legend(title="",keywidth = 3, keyheight = 1.5), 
           colour = guide_legend(keywidth = 3, keyheight = 1.5))+
    expand_limits(x = 1, y = 0)+ theme(text=element_text(size=22,  family="Times"))+
    labs(x = "Processors", y = "Normalized Gap Closed", title = paste0(plot_title,"_",learnertype,"_",se.method))+
    theme(plot.title = element_text(hjust = 0.5))+
    scale_x_continuous(breaks=c(1:max_core)) +
    scale_color_discrete(labels=c("3S",bquote(AS[0]), 'SBS',"Time Splitting",
                                  bquote(AS[~p[~'\u2229']]), bquote(AS[KL]), 'VBS'))+
    scale_shape_discrete(labels=c("3S",bquote(AS[0]), 'SBS',"Time Splitting",
                                  bquote(AS[~p[~'\u2229']]), bquote(AS[KL]), 'VBS'))
  if(save){
    ggsave(dpi = 500, width = 9, height = 5, filename = paste0(basepath, benchmark_name,"_line_chart_parallel_NormalizedGap.pdf"))
  }
  return(p)
}

plot_overlap_area = function(){
  self = PredictionResults$new(benchmarks_name = benchmark_name, cluster = cluster, learnertype = learnertype, se.method = se.method)
  create_directory_if_not_exist(paste0(predictionPath, "overlap_area_plots/"))
  for(instance in self$sequentialData$instances){
    if(benchmark_name == "SAT2016" || benchmark_name == "SAT2018"){
      instance = str_split(instance,"sat/")[[1]][2]
    }
    if(combined == "combined"){
      pred = read.csv(paste0(predictionPath,"preds_combined/",instance,".csv"))
    } else if (combined == "seperate") {
      pred = read.csv(paste0(predictionPath,"preds/",instance,".csv"))
    }
    p = self$plot_overlap_area(pred)
   
    ggsave(paste0(predictionPath, "overlap_area_plots/",instance,".pdf"), plot = p, width = 10, height = 8, dpi = 300)
  }
}

# plot_overlap_area()

# set_paths()

# results = scrape_all_results()
# results$scenario = benchmark_name

# write.csv(results,paste0(basepath,'summary_results_all_',benchmark_name,'.csv'),row.names = FALSE)

# p = plot_all_results_normalized_gap(TRUE)
# p
# ggsave(dpi = 500, width = 9, height = 5, filename = paste0(basepath,benchmark_name,"_line_chart_parallel_NormalizedGap.pdf"))

# ggsave(dpi = 500, width = 7.5, height = 4, filename = paste0(basepath,benchmark_name,"_line_chart_parallel_NormalizedGap.svg"))
# p = plot_all_results(metric = "Runtime")
# p
# ggsave(dpi = 500, width = 9, height = 5, filename = paste0(basepath,benchmark_name,"_summary_results_all_runtime.pdf"))
# ggsave(dpi = 500, width = 7.5, height = 4, filename = paste0(basepath,benchmark_name,"_summary_results_all_runtime.svg"))

# s = plot_all_results(metric = "MCP")
# s
# ggsave(dpi = 500, width = 9, height = 5, filename = paste0(basepath,benchmark_name,"_summary_results_all_MCP.pdf"))
# ggsave(dpi = 500, width = 7.5, height = 4, filename = paste0(basepath,benchmark_name,"_summary_results_all_MCP.svg"))

# o = plot_all_results(metric = "PAR10")
# o
# ggsave(dpi = 500, width = 9, height = 5, filename = paste0(basepath,benchmark_name,"_summary_results_all_PAR10.pdf"))
# ggsave(dpi = 500, width = 7.5, height = 4, filename = paste0(basepath,benchmark_name,"_summary_results_all_PAR10.svg"))
