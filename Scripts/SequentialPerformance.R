if(Sys.info()['sysname']=="Linux"){
  if(file.exists("/home/haniye/Documents/nextSteps/Scripts/ParallelLevel.R")){
    source("/home/haniye/Documents/nextSteps/Scripts/ParallelLevel.R") 
  }
  else{ 
    source("/gscratch/hkashgar/nextSteps/Scripts/ParallelLevel.R") 
  }
} else{
  source("C:/Users/hnyk9/Thesis/nextSteps/Scripts/ParallelLevel.R")
}

SequentialPerformance <- R6Class(
  classname = "SequentialPerformance",
  inherit = ParallelLevel,
  public = list(
    features_path = NULL,
    features = NULL, 
    scenario_path = NULL,
    scenario = NULL,
    llamaData = NULL,
    vbsData = NULL,
    sbsSolver = NULL,
    unsolvedInstances = NULL,
    solvedInstances = NULL,
    cluster = FALSE,
    benchmarks_name = NULL,
    #constructor
    
    initialize = function(benchmarks_name="SAT2018", cluster=FALSE) {
      self$cluster <- cluster
      self <- super$initialize(cores = 1, benchmarks_name = benchmarks_name, cluster = cluster)
      paths <- self$set_scenario_and_features_paths(benchmarks_name, cluster)
      
      self$scenario_path <- paths$scenario_path
      self$features_path <- paths$features_path
      
      invisible(self)
    },
    
    set_scenario_and_features_paths = function(benchmark_name, cluster) {
      base_dir = ""
      if (Sys.info()['sysname'] != "Linux") {
        base_dir = "C:/Users/hnyk9/Thesis/nextSteps/ExperimentResults/"
      } else if (cluster) {
        base_dir = "/gscratch/hkashgar/nextSteps/ExperimentResults/"
      } else {
        base_dir = "/home/haniye/Documents/nextSteps/ExperimentResults/"
      }
      folder_map <- list(
        SAT2018 = "SAT18/SAT18_EXP",
        SAT2016 = "SAT16/SAT16-MAIN",
        GRAPHS2015 = "Portfolio-Scheduling/originalCSVs-Teton_GRAPHS15/GRAPHS-2015",
        MAXSAT2019 = "MAXSAT19/MAXSAT-2019",
        IPC2018 = "IPC2018/IPC2018",
        `SAT11-INDU` = "SAT11-INDU/SAT11-INDU",
        IPC2018_Parallel = "IPC2018/IPC2018_Parallel",
        IPC2018_Parallel_1 = "IPC2018/IPC2018",
        IPC2018_Parallel_2 = "IPC2018/IPC2018_Parallel_2",
        IPC2018_Parallel_3 = "IPC2018/IPC2018_Parallel_3",
        IPC2018_Parallel_4 = "IPC2018/IPC2018_Parallel_4",
        IPC2018_Parallel_5 = "IPC2018/IPC2018_Parallel_5",
        IPC2018_Parallel_6 = "IPC2018/IPC2018_Parallel_6",
        IPC2018_Parallel_7 = "IPC2018/IPC2018_Parallel_7",
        IPC2018_Parallel_8 = "IPC2018/IPC2018_Parallel_8",
        IPC2018_Parallel_9 = "IPC2018/IPC2018_Parallel_9",
        IPC2018_Parallel_10 = "IPC2018/IPC2018_Parallel_10",
        SAT2016_Parallel = "SAT16/SAT16-MAIN_Parallel",
        SAT2018_Parallel = "SAT18/SAT18_EXP_Parallel",
        `SAT11-INDU_Parallel` = "SAT11-INDU/SAT11-INDU_Parallel",
        MAXSAT2019_Parallel = "MAXSAT19/MAXSAT-2019_Parallel",
        MAXSAT2019_Parallel_1 = "MAXSAT19/MAXSAT-2019",
        MAXSAT2019_Parallel_2 = "MAXSAT19/MAXSAT-2019_Parallel_2",
        MAXSAT2019_Parallel_3 = "MAXSAT19/MAXSAT-2019_Parallel_3",
        MAXSAT2019_Parallel_4 = "MAXSAT19/MAXSAT-2019_Parallel_4",
        MAXSAT2019_Parallel_5 = "MAXSAT19/MAXSAT-2019_Parallel_5",
        MAXSAT2019_Parallel_6 = "MAXSAT19/MAXSAT-2019_Parallel_6",
        MAXSAT2019_Parallel_7 = "MAXSAT19/MAXSAT-2019_Parallel_7",
        `SAT11-INDU_Parallel_1` = "SAT11-INDU/SAT11-INDU",
        `SAT11-INDU_Parallel_2` = "SAT11-INDU/SAT11-INDU_Parallel_2",
        `SAT11-INDU_Parallel_3` = "SAT11-INDU/SAT11-INDU_Parallel_3",
        `SAT11-INDU_Parallel_4` = "SAT11-INDU/SAT11-INDU_Parallel_4",
        `SAT11-INDU_Parallel_5` = "SAT11-INDU/SAT11-INDU_Parallel_5",
        `SAT11-INDU_Parallel_6` = "SAT11-INDU/SAT11-INDU_Parallel_6",
        `SAT11-INDU_Parallel_7` = "SAT11-INDU/SAT11-INDU_Parallel_7",
        `SAT11-INDU_Parallel_8` = "SAT11-INDU/SAT11-INDU_Parallel_8",
        `SAT11-INDU_Parallel_9` = "SAT11-INDU/SAT11-INDU_Parallel_9",
        `SAT11-INDU_Parallel_10` = "SAT11-INDU/SAT11-INDU_Parallel_10",
        SAT2016_Parallel_1 = "SAT16/SAT16-MAIN",
        SAT2016_Parallel_2 = "SAT16/SAT16-MAIN_Parallel_2",
        SAT2016_Parallel_3 = "SAT16/SAT16-MAIN_Parallel_3",
        SAT2016_Parallel_4 = "SAT16/SAT16-MAIN_Parallel_4",
        SAT2016_Parallel_5 = "SAT16/SAT16-MAIN_Parallel_5",
        SAT2016_Parallel_6 = "SAT16/SAT16-MAIN_Parallel_6",
        SAT2016_Parallel_7 = "SAT16/SAT16-MAIN_Parallel_7",
        SAT2016_Parallel_8 = "SAT16/SAT16-MAIN_Parallel_8",
        SAT2016_Parallel_9 = "SAT16/SAT16-MAIN_Parallel_9",
        SAT2016_Parallel_10 = "SAT16/SAT16-MAIN_Parallel_10",
        SAT2018_Parallel_1 = "SAT18/SAT18_EXP",
        SAT2018_Parallel_2 = "SAT18/SAT18_EXP_Parallel_2",
        SAT2018_Parallel_3 = "SAT18/SAT18_EXP_Parallel_3",
        SAT2018_Parallel_4 = "SAT18/SAT18_EXP_Parallel_4",
        SAT2018_Parallel_5 = "SAT18/SAT18_EXP_Parallel_5",
        SAT2018_Parallel_6 = "SAT18/SAT18_EXP_Parallel_6",
        SAT2018_Parallel_7 = "SAT18/SAT18_EXP_Parallel_7",
        SAT2018_Parallel_8 = "SAT18/SAT18_EXP_Parallel_8",
        SAT2018_Parallel_9 = "SAT18/SAT18_EXP_Parallel_9",
        SAT2018_Parallel_10 = "SAT18/SAT18_EXP_Parallel_10"
      )
      
      scenario_folder <- folder_map[[benchmark_name]]
      scenario_path <- paste0(base_dir, "aslib_scenarios/", scenario_folder, "/")
      if(stringr::str_detect(benchmark_name, "_Parallel_")){
        features_path <- paste0(base_dir, "csvs/", strsplit(benchmark_name,"_")[[1]][1],"/",strsplit(benchmark_name,"_")[[1]][1], "_features.csv")
      } else{
        features_path <- paste0(base_dir, "csvs/", benchmark_name,"/",benchmark_name, "_features.csv")
      }
      
      return(list(scenario_path = scenario_path, features_path = features_path))
    },
    
    
    #get scenario, scenario path is predefined 
    get_scenario = function(){
      if(is.null(self$scenario)){
        self$scenario = parseASScenario(self$scenario_path)
        #convert scenario to llama data format
        self$llamaData = convertToLlamaCVFolds(self$scenario)
      }
      return(self$scenario)
    },
    
    #get instance features, path of the csv is predefined
    get_features = function(){
      if(is.null(self$features) || nrow(self$features) == 0){
        self$features = read.csv(self$features_path)
        if(!is.null(self$features$benchmark))
          self$features = self$features[order(self$features$benchmark),]
        if(!is.null(self$features$InstanceName))
          self$features = self$features[order(self$features$InstanceName),]
      }
      return(self$features)
    },
    
    #get vbs solvers and runtime per instance    
    get_VBS = function(){
      #get min runtimes
      VBS_Runtime = rowMins(as.matrix(self$actual_CSV[,2:(self$n_solvers+1)]),na.rm = FALSE)
      #get solvers which gives minimum runtimes
      VBS_Solvers = colnames(self$actual_CSV)[apply(self$actual_CSV,1,which.min)]
      #combine both to csv
      vbs = data.frame(self$actual_CSV$InstanceName, VBS_Solvers,VBS_Runtime)
      colnames(vbs)[1] <- "InstanceName"
      #set the props in the class
      self$vbsData = vbs
      self$solvedInstances = subset(vbs,vbs$VBS_Runtime<self$Cutoff )$InstanceName
      if(is.null(self$unsolvedInstances)) self$unsolvedInstances = subset(vbs,vbs$VBS_Runtime>=self$Cutoff )$InstanceName
      return(self$vbsData)
    },
    
    #get vbs's par10
    get_VBS_par10 = function(){
      vbs = self$get_VBS()
      vbs$VBS_Runtime[which(vbs$VBS_Runtime>=self$Cutoff )]<- (self$Cutoff * 10)
      print(summary(vbs$VBS_Runtime))
      return(vbs)
    },
    
    #get vbs's mcp
    get_VBS_mcp = function(){
      return("MCP for VBS is always zero!")
    },
    
    #get instances solved by vbs
    get_solved_instances = function(){
      self$get_VBS()
      return(self$solvedInstances)
    },
    
    #get instances not solved by vbs
    get_unsolved_instances = function(){
      self$get_VBS()
      return(self$unsolvedInstances)
    },
    
    #get SBS for all instances
    get_SBS = function(){
      self$get_scenario()
      self$sbsSolver = as.vector(unique(singleBest(self$llamaData)$algorithm))
      return(self$sbsSolver)
    },
    
    #input = solver name, output = runtimes for solved cases
    get_solvers_solved_runtime = function(solver){
      runtimes = self$actual_CSV[solver]
      return(runtimes[runtimes<self$Cutoff])
    },
    
    #method to use when we want to ignore the instances solved by vbs, overrided from top
    ignore_instances = function(unsolvedinstances=NULL){
      self$get_mcp_dataframe(self)
      #get unsolved instances if not provided
      if(is.null(unsolvedinstances)) {
        self$unsolvedInstances = self$get_unsolved_instances()
        unsolvedinstances = self$unsolvedInstances
      }
      #run parent's method
      super$ignore_instances(unsolvedinstances)
      
      #get features and ignore unsolved instance features
      if(is.null(self$features) || nrow(self$features) == 0) self$get_features()
      self$features = subset(self$features, !(self$features$benchmark %in% unsolvedinstances))
      #get scenario and ignore instances in scenario
      if(is.null(self$scenario)) self$get_scenario()
      self$scenario$algo.runs = subset(self$scenario$algo.runs,!(self$scenario$algo.runs$instance_id %in% unsolvedinstances))
      self$scenario$algo.runstatus = subset(self$scenario$algo.runstatus,!(self$scenario$algo.runstatus$instance_id %in% unsolvedinstances))
      self$scenario$cv.splits = subset(self$scenario$cv.splits,!(self$scenario$cv.splits$instance_id %in% unsolvedinstances))
      self$scenario$feature.values = subset(self$scenario$feature.values,!(self$scenario$feature.values$instance_id %in% unsolvedinstances))
      self$scenario$feature.costs = subset(self$scenario$feature.costs,!(self$scenario$feature.costs$instance_id %in% unsolvedinstances))
      self$scenario$cv.splits = subset(self$scenario$cv.splits,!(self$scenario$cv.splits$instance_id %in% unsolvedinstances))
      self$scenario$feature.runstatus = subset(self$scenario$feature.runstatus,!(self$scenario$feature.runstatus$instance_id %in% unsolvedinstances))
      self$llamaData = aslib::convertToLlamaCVFolds(self$scenario)
      #get vbs, get sbs
      self$get_VBS()
      self$get_SBS()
      self$n_instances = nrow(self$actual_CSV)
      invisible(self)
    },
    
    # Get the n-th best solver (VBS) and its runtime for each instance
    # Based on only sequential's actual results
    get_nd_VBS = function(nd = 1){
      # Apply a function to each row (instance) to find the n-th minimum runtime.
      # This is done by sorting the runtimes for each instance and selecting the n-th value.
      Runtime = apply(self$actual_CSV[2:ncol(self$actual_CSV)],1, FUN = function(x) sort(x)[nd])
 
      # Determine the solvers that correspond to these n-th minimum runtimes for each instance.
       Solvers = colnames(self$actual_CSV)[sapply(1:nrow(self$actual_CSV),FUN = function(x) which(self$actual_CSV[x,1:ncol(self$actual_CSV)] == Runtime[x])[[1]])]
       
      # Combine the extracted information into a new data frame.
      # This data frame includes the instance names, the n-th VBS for each instance, and the corresponding n-th minimum runtime.    
      res = data.frame(self$actual_CSV$InstanceName, Solvers,Runtime)
      
      colnames(res)[1] <- "InstanceName"
      return(res)
    },
    
    #train random forest, tuned and cv10
    train_aslibLike = function(savepath, ignoreTimeouts, train_by_par10=FALSE, learnertype = "ranger", se.method = "", log10 = FALSE){
      Tools$new()$create_directory_if_not_exists(paste0(savepath))
      Tools$new()$create_directory_if_not_exists(paste0(savepath,"/",learnertype))
      Tools$new()$create_directory_if_not_exists(paste0(savepath,"/",learnertype,"/",se.method))
      
      savepath = paste0(savepath,"/",learnertype,"/",se.method)
      asscenario = self$get_scenario()
      
      if(se.method != ""){
        learner = lrn(paste0("regr.",learnertype),predict_type = "se", se.method = se.method)
      } else{
        learner = lrn(paste0("regr.",learnertype),predict_type = "se")
      }
      
      par.set = makeParamSet(
        makeIntegerParam(paste0("num.trees"), lower = 10, upper = 200),
        makeIntegerParam(paste0("mtry"), lower = 1, upper = 30)
      )
      wd = getwd()
      setwd(savepath)
      rs.iters = asInt(250L, lower = 1L)
      n.inner.folds = asInt(3L, lower = 2L)
      llama.scenario = convertToLlama(asscenario = asscenario, feature.steps = 'ALL')
      llama.cv = self$llamaData
      desc = asscenario$desc
      cutoff = desc$algorithm_cutoff_time
      timeout = if (desc$performance_type[[1L]] == "runtime" && !is.na(cutoff)) {
        cutoff
      } else {
        NULL
      }
      n.algos = length(getAlgorithmNames(asscenario))
      pre = function(x, y = NULL) {
        list(features = x)
      }
      n.outer.folds = length(llama.cv$test)
      outer.preds = vector("list", n.outer.folds)
      ldf = llama.cv
      
      if(!"missings" %in% learner$properties){
        impute_pipeline <- gunion(list(
          po("imputemean", id = "impute_numeric", param_vals = list(affect_columns = selector_type("numeric"))),
          po("imputemean", id = "impute_integer", param_vals = list(affect_columns = selector_type("integer"))),
          po("imputemode", id = "impute_logical", param_vals = list(affect_columns = selector_type("logical"))),
          po("imputeconstant", id = "impute_factor", param_vals = list(constant = "NA", affect_columns = selector_type("factor"))),
          po("imputeconstant", id = "impute_character", param_vals = list(constant = "NA", affect_columns = selector_type("character")))
        ))
        task = TaskRegr$new(id = "imputed_task", backend = ldf$data, target = "ALL")
        data_df = impute_pipeline$train(task)[[1]]$data()
        ldf$data = as.data.frame(data_df)
      }
      
      # des = data.frame(matrix(nrow=0,ncol=2))
      # des = rbind(des, c(113,23))
      # des = rbind(des, c(20,22))
      # des = rbind(des, c(166,26)) >> >>
      # des = rbind(des, c(187,27)) >>
      # des = rbind(des, c(29,30)) >> >> >>
      # des = rbind(des, c(48,8))
      # des = rbind(des, c(68,24))
      # des = rbind(des, c(154,22)) >> >> >>
      # des = rbind(des, c(177,21))
      # des = rbind(des, c(187,15)) >>
      # colnames(des) = c("ntree","mtry")
      
      for (i in 1:n.outer.folds) {
        ldf2 = ldf
        ldf2$data = ldf$data[ldf$train[[i]],]
        ldf2$train = NULL
        ldf2$test = NULL
        ldf3 = cvFolds(ldf2, nfolds = n.inner.folds, stratify = FALSE)
        
        # if(learnertype!="km"){
          des = ParamHelpers::generateRandomDesign(rs.iters, par.set, trafo = TRUE)
          des.list = ParamHelpers::dfRowsToList(des, par.set)  
        # } 
        
        parallelStartMulticore(cpus = (detectCores()-1))
        ys = parallelMap(function(x) {
          par10 = try({
            do.call(function(...) learner$param_set$set_values(...), x)
            p = regression(regressor = learner, data = ldf3, pre = pre)
            ldf4 = fixFeckingPresolve(asscenario, ldf3)
            par10 = mean(parscores(ldf4, p, timeout = timeout))
            messagef("[Tune]: %s : par10 = %g", ParamHelpers::paramValueToString(par.set, x), par10)
            return(par10)
          })
          if(inherits(par10, "try-error")) {
            par10 = NA
          }
          return(par10)
        }, des.list, simplify = TRUE)
        parallelStop()
        best.i = getMinIndex(ys)
        best.parvals = des.list[[best.i]]
        print(best.parvals)
        messagef("[Best]: %s : par10 = %g", ParamHelpers::paramValueToString(par.set, best.parvals), ys[best.i])
        parvals= best.parvals
        
        learner2 = learner
        do.call(function(...) learner2$param_set$set_values(...), parvals) 
        outer.split.ldf = ldf
        outer.split.ldf$train = list(ldf$train[[i]])
        outer.split.ldf$test = list(ldf$test[[i]])
        outer.preds[[i]] = regression(learner2, data = outer.split.ldf, pre = pre, save.models = TRUE)
        outer.preds[[i]]$train = outer.split.ldf$train 
        outer.preds[[i]]$test = outer.split.ldf$test 
      }
      
      retval = outer.preds[[1]]
      retval$predictions = do.call(rbind, lapply(outer.preds, function(x) { x$predictions }))
      if(!is.null(retval$models[[1]]$learner$id)){
        saveRDS(retval, file = paste0(savepath,"/",strsplit(retval$models[[1]]$learner$id,"\\.")[[1]][2],"_predictions.RDS"))
        saveRDS(outer.preds, file = paste0(savepath,"/",strsplit(retval$models[[1]]$learner$id,"\\.")[[1]][2],"_all.RDS"))
      } else {
        saveRDS(retval, file = paste0(savepath,"/",strsplit(retval$models[[1]]$id,"\\.")[[1]][2],"_predictions.RDS"))
        saveRDS(outer.preds, file = paste0(savepath,"/",strsplit(retval$models[[1]]$id,"\\.")[[1]][2],"_all.RDS"))
      }
      return(retval) 
    },
    
    train_aslibLike_km = function(savepath, ignoreTimeouts, train_by_par10=FALSE, learnertype = "ranger", se.method = "", log10 = FALSE, impute_pipeline){
      Tools$new()$create_directory_if_not_exists(paste0(savepath))
      Tools$new()$create_directory_if_not_exists(paste0(savepath,"/",learnertype))
      Tools$new()$create_directory_if_not_exists(paste0(savepath,"/",learnertype,"/",se.method))
      
      savepath = paste0(savepath,"/",learnertype,"/",se.method)
      asscenario = self$get_scenario()
      
      learner = lrn(paste0("regr.",learnertype),predict_type = "se")
      
      par.set = makeParamSet(
        makeDiscreteParam("covtype", "gauss"),
        makeNumericParam("nugget.stability", lower = 1e-10, upper = 1e-6), # Continuous range
        makeNumericParam("jitter", lower = 1e-15, upper = 1e-10) # Small jitter range for stability
        # makeDiscreteParam("type", "UK")
      )
      
      wd = getwd()
      setwd(savepath)
      rs.iters = asInt(250L, lower = 1L)
      # rs.iters = asInt(1L, lower = 1L)
      n.inner.folds = asInt(3L, lower = 2L)
      # n.inner.folds = asInt(1L, lower = 1L)
      llama.scenario = convertToLlama(asscenario = asscenario, feature.steps = 'ALL')
      llama.cv = self$llamaData
      desc = asscenario$desc
      cutoff = desc$algorithm_cutoff_time
      timeout = if (desc$performance_type[[1L]] == "runtime" && !is.na(cutoff)) {
        cutoff
      } else {
        NULL
      }
      n.algos = length(getAlgorithmNames(asscenario))
      pre = function(x, y = NULL) {
        list(features = x)
      }
      n.outer.folds = length(llama.cv$test)
      outer.preds = vector("list", n.outer.folds)
      ldf = llama.cv
      
      if(!"missings" %in% learner$properties){
        task = TaskRegr$new(id = "imputed_task", backend = ldf$data, target = "ALL")
        data_df = impute_pipeline$train(task)[[1]]$data()
        ldf$data = as.data.frame(data_df)
      }
      
        # task = TaskRegr$new(id = "imputed_task", backend = ldf$data, target = "ALL")
        # data_df = impute_pipeline$train(task)[[1]]$data()
        # ldf$data = as.data.frame(data_df)

      for (i in 1:n.outer.folds) {
        ldf2 = ldf
        ldf2$data = ldf$data[ldf$train[[i]],]
        ldf2$train = NULL
        ldf2$test = NULL
        ldf3 = cvFolds(ldf2, nfolds = n.inner.folds, stratify = FALSE)
        
        # if(learnertype!="km"){
        set.seed(1234)
        des = ParamHelpers::generateRandomDesign(rs.iters, par.set, trafo = TRUE)
        des.list = ParamHelpers::dfRowsToList(des, par.set)
        # } 
        
        parallelStartMulticore(cpus = (detectCores()-1))
        ys = parallelMap(function(x) {
          par10 = try({
            do.call(function(...) learner$param_set$set_values(...), x)
            p = regression(regressor = learner, data = ldf3, pre = pre)
            ldf4 = fixFeckingPresolve(asscenario, ldf3)
            par10 = mean(parscores(ldf4, p, timeout = timeout))
            messagef("[Tune]: %s : par10 = %g", ParamHelpers::paramValueToString(par.set, x), par10)
            return(par10)
          })
          if(inherits(par10, "try-error")) {
            par10 = NA
          }
          return(par10)
        }, des.list, simplify = TRUE)
        parallelStop()
        best.i = getMinIndex(ys)
        best.parvals = des.list[[best.i]]
        print(best.parvals)
        messagef("[Best]: %s : par10 = %g", ParamHelpers::paramValueToString(par.set, best.parvals), ys[best.i])
        parvals= best.parvals
        
        learner2 = learner
        do.call(function(...) learner2$param_set$set_values(...), parvals) 
        outer.split.ldf = ldf
        outer.split.ldf$train = list(ldf$train[[i]])
        outer.split.ldf$test = list(ldf$test[[i]])
        outer.preds[[i]] = regression(learner2, data = outer.split.ldf, pre = pre, save.models = TRUE)
        outer.preds[[i]]$train = outer.split.ldf$train 
        outer.preds[[i]]$test = outer.split.ldf$test 
      }
      
      retval = outer.preds[[1]]
      retval$predictions = do.call(rbind, lapply(outer.preds, function(x) { x$predictions }))
      if(!is.null(retval$models[[1]]$learner$id)){
        saveRDS(retval, file = paste0(savepath,"/",strsplit(retval$models[[1]]$learner$id,"\\.")[[1]][2],"_predictions.RDS"))
        saveRDS(outer.preds, file = paste0(savepath,"/",strsplit(retval$models[[1]]$learner$id,"\\.")[[1]][2],"_all.RDS"))
      } else {
        saveRDS(retval, file = paste0(savepath,"/",strsplit(retval$models[[1]]$id,"\\.")[[1]][2],"_predictions.RDS"))
        saveRDS(outer.preds, file = paste0(savepath,"/",strsplit(retval$models[[1]]$id,"\\.")[[1]][2],"_all.RDS"))
      }
      return(retval) 
    }
    
    #I guess it can be removed, it's an old function
    #split by solver all actual
    # split_dataframe_by_solver = function(saveto = NULL){
    #   path = self$actual_CSV_path
    #   path = str_split(path,"/")[[1]]
    #   path = path[-length(path)]
    #   path = paste0(path, collapse = '/')
    #   if(self$benchmarks_name == "SAT2018"){
    #     csvs = list.files(path,"replacement.csv",full.names = TRUE)
    #   } else{
    #     csvs = list.files(path,".csv",full.names = TRUE)
    #   } 
    #   cores = c(1:10,20,30,32)
    #   solvercsv = data.frame()
    #   for(i in 2:self$n_solvers){
    #     colnames = vector()
    #     colnames = append(colnames,"InstanceName")
    #     solver = colnames(self$actual_CSV[i])
    #     solvercsv = self$actual_CSV[1]
    #     for(core in cores){
    #       if(core == 1){ 
    #         csv = csvs[which(grepl(csvs,pattern = "-solo-"))]
    #         core = "1-parallel"
    #       } else {
    #         csv = csvs[which(grepl(csvs,pattern = paste0("-",core,"-parallel")))]
    #         core = paste0(core,"-parallel" )
    #       }
    #       readcsv = read.csv(csv)
    #       readcsv = readcsv[order(readcsv$InstanceName),]
    #       colnames = append(colnames,core)
    #       solvercsv = cbind(solvercsv,readcsv[solver])
    #     }
    #     colnames(solvercsv) <- colnames
    #     if (is.null(saveto)) { # If saveto is NULL, return the list of dataframes
    #       return(solvers_data)
    #     }
    #     if(!is.null(saveto)){
    #       write.csv(solvercsv,paste(saveto,solver,".csv"),row.names = FALSE)
    #     } 
    #   }
    #   if (is.null(saveto)) { # If saveto is NULL, return the list of dataframes
    #     return(solvers_data)
    #   }
    # }
  )
)

