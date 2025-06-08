if(Sys.info()['sysname']=="Linux"){
  if(file.exists("/home/haniye/Documents/nextSteps/Scripts/SequentialPerformance.R")){
    source("/home/haniye/Documents/nextSteps/Scripts/SequentialPerformance.R") 
  }
  else{ 
    source("/gscratch/hkashgar/nextSteps/Scripts/SequentialPerformance.R") 
  }
} else{
  source("C:/Users/hnyk9/Thesis/nextSteps/Scripts/SequentialPerformance.R")
}


PredictionResults = R6Class(
  classname = "PredictionResults",
  public = list(
    sequentialData = NULL,
    benchmarks_name = NULL,
    predictionPath = NULL,
    modelPath = NULL,
    selectionPath = NULL,
    cluster = FALSE,
    cpus = (detectCores()-1),
    learnertype = NULL,
    se.method = NULL,
    combined_model = "",
    initialize = function(benchmarks_name="SAT2018", cluster = FALSE, learnertype = "ranger", se.method = "", combined_model = "combined"){
      if (Sys.info()['sysname'] == "Linux") {
        base_path <- ifelse(cluster, "/gscratch/hkashgar/", "/home/haniye/Documents/")
      } else {
        base_path <- "C:/Users/hnyk9/Thesis/"
      }
      
      if(se.method != ""){
        self$predictionPath <- paste0(base_path, "ranger_results_",combined_model,"/", benchmarks_name, "/", learnertype, "/", se.method, "/")
        self$modelPath <- paste0(base_path, "mlr-scripts/", benchmarks_name, "/Prediction/StandardError/", learnertype,"/", se.method, "/")
        self$selectionPath <- paste0(base_path, "ranger_results_",combined_model,"/", benchmarks_name, "/", learnertype, "/", se.method, "/")  
      } else{
        self$predictionPath <- paste0(base_path, "ranger_results_",combined_model,"/", benchmarks_name, "/", learnertype, "/")
        self$modelPath <- paste0(base_path, "mlr-scripts/", benchmarks_name, "/Prediction/StandardError/", learnertype, "/")
        self$selectionPath <- paste0(base_path, "ranger_results_",combined_model,"/", benchmarks_name, "/", learnertype, "/")  
      }
      
      self$benchmarks_name <- benchmarks_name
      self$sequentialData <- SequentialPerformance$new(benchmarks_name, cluster)
      self$cluster <- cluster
      self$learnertype <- learnertype
      self$se.method <- se.method
      if (is.null(self$sequentialData$scenario)) {
        self$sequentialData$get_scenario()
      }
      invisible(self)
    },  
    
    #get the models' prediction for test set
    get_models_prediction = function(modelPath = self$modelPath, savePath = self$predictionPath,train_by_par10=FALSE){
      modelPath = paste0(modelPath,"/",self$learnertype,"_predictions.RDS")
      print(modelPath)
      predictions = readRDS(modelPath)$predictions
      instances = self$sequentialData$llamaData$data$instance_id
      #instance ids are ordered as 
      parallelStartSocket(cpus = self$cpus)
      # print(savePath)
      for(i in 1:length(instances)){
        Tools$new()$create_directory_if_not_exists(paste0(savePath))
        Tools$new()$create_directory_if_not_exists(paste0(savePath, "preds/"))
        if(startsWith(self$benchmarks_name,"SAT2")){
          if(!file.exists(paste0(savePath,"preds/",str_split(instances[i],"sat/")[[1]][2],".csv"))){
            data = predictions[which(predictions$instance_id==instances[i]),]
            data = data[-5]
            rowTruth = self$sequentialData$get_actual_result_csv()
            rowTruth = rowTruth[which(rowTruth$InstanceName==instances[i]),]
            Truth <- melt(rowTruth, id.vars = c("InstanceName"), variable_name = "Solver")
            colnames(Truth)[2] = "Solver"
            Truth = Truth[order(Truth$Solver),]
            data = data[order(data$algorithm),]
            #print(data$algorithm == Truth$Solver)
            data = add_column(data,Truth$value,.after = "algorithm")
            #check if data is par10
            if(train_by_par10) {colnames(data)<-c("InstanceName","Solver","ActualPar10","PredictedPar10","Prediction_StandardError")}
            else {colnames(data)<-c("InstanceName","Solver","ActualRuntime","PredictedRuntime","Prediction_StandardError")}
            write.csv(data,paste0(savePath,"preds/",str_split(instances[i],"sat/")[[1]][2],".csv"), row.names = FALSE)
          }
          else{
            print(paste0("File ",paste0(savePath,"preds/",str_split(instances[i],"sat/")[[1]][2],".csv"), " exist!"))
          }
        } else {
          if(!file.exists(paste0(savePath,"preds/",instances[i],".csv"))){
            data = predictions[which(predictions$instance_id==instances[i]),]
            data = data[-5]
            rowTruth = self$sequentialData$get_actual_result_csv()
            rowTruth = rowTruth[which(rowTruth$InstanceName==instances[i]),]
            Truth <- melt(rowTruth, id.vars = c("InstanceName"), variable_name = "Solver")
            colnames(Truth)[2]<-"Solver"
            Truth = Truth[order(Truth$Solver),]
            data = data[order(data$algorithm),]
            #print(data$algorithm == Truth$Solver)
            data = add_column(data,Truth$value,.after = "algorithm")
            #check if data is par10
            if(train_by_par10) {colnames(data)<-c("InstanceName","Solver","ActualPar10","PredictedPar10","Prediction_StandardError")}
            else {colnames(data)<-c("InstanceName","Solver","ActualRuntime","PredictedRuntime","Prediction_StandardError")}
            write.csv(data,paste0(savePath,"preds/",instances[i],".csv"), row.names = FALSE)
          }
          else{
            print(paste0("File ",paste0(savePath,"preds/",str_split(instances[i],"sat/")[[1]][2],".csv"), " exist!"))
          }
        }
      }
      parallelStop()
      invisible(self)
    },

    #probability of having vbs selected in the folds 
    get_VBS_probability_folds = function(CVsetsPath,saveTo){
      self$sequentialData$ignore_instances()
      instances = self$sequentialData$solvedInstances
      solvers = self$sequentialData$solvers
      vbs = self$sequentialData$get_VBS()
      #instance ids are ordered as 
      parallelStartSocket(cpus = self$cpus)
      count_vbs = data.frame(matrix(nrow=0,ncol=40))
      for(i in 1:length(instances)){
        if(file.exists(paste0(CVsetsPath,"/",
                              str_split(instances[i],"sat/")[[1]][2],".csv"))){
          data = read.csv(paste0(CVsetsPath,"/",
                                 str_split(instances[i],"sat/")[[1]][2],".csv"))
          instance_preds = data[which(data$InstanceName == instances[i]),]
          vbs_ins = vbs[which(vbs$InstanceName==instances[i]),]
          counts = rep(0,40)
          counts[1] = instances[i]
          for(iter in 1:10){
            set = data[which(data$Iteration==iter),]
            set = set[order(set$PredictedRuntime),]
            for(j in 1:39){
              if(set[j,]$Solver == vbs_ins$VBS_Solvers){
                counts[j+1] = as.numeric(counts[j+1])+1
              }
            }
          }
          count_vbs = rbind(count_vbs,counts)
        }
      }
      colnames(count_vbs) <- c("InstanceName",c(1:39))
      count_vbs_cumulative = data.frame(matrix(nrow=nrow(count_vbs),ncol=40))
      count_vbs_cumulative[1] = count_vbs$InstanceName
      count_vbs_cumulative[2] = count_vbs[,2]
      for(col in 3:ncol(count_vbs)){
        count_vbs_cumulative[col] = as.numeric(count_vbs[,col])+
          as.numeric(count_vbs_cumulative[,col-1])
      }
      count_vbs_cumulative[2:40] = sapply(count_vbs_cumulative[2:40] ,as.numeric)
      count_vbs_cumulative[2:40] = count_vbs_cumulative[2:40] /10
      colnames(count_vbs_cumulative) <- c("InstanceName",c(1:39))
      if(!is.null(saveTo)){
        write.csv(count_vbs_cumulative,paste0(saveTo,"/VBS_occurance_cumulative.csv"),row.names = FALSE)
      }
      parallelStop()
      return(count_vbs_cumulative)
    },
    
    #probability of having vbs selected in all instances prediction
    get_VBS_probability_all_instances = function(predictionPath = self$predictionPath,saveTo=""){
      self$sequentialData$ignore_instances()
      instances = self$sequentialData$solvedInstances
      solvers = self$sequentialData$solvers
      vbs = self$sequentialData$get_VBS()
      count_vbs = data.frame(matrix(nrow=0,ncol=length(solvers)+1))
      parallelStartSocket(cpus = self$cpus)
      for(instance in vbs$InstanceName){
        preds = read.csv(paste0(predictionPath,"/",
                                str_split(instance,"sat/")[[1]][2],".csv"))
        preds = preds[order(preds$PredictedRuntime),]
        solver_order = preds$Solver
        vbs_solver = vbs[which(vbs$InstanceName ==instance),"VBS_Solvers"]
        counts = rep(0,length(solvers)+1)
        counts[1] = instance
        for(j in 1:length(solvers)){
          if(solver_order[j] == vbs_solver){
            counts[j+1] = as.numeric(counts[j+1])+1
          }
        }
        count_vbs = rbind(count_vbs,counts)
      }
      colnames(count_vbs) <- c("InstanceName",c(1:length(solvers)))
      count_vbs_cumulative = data.frame(matrix(nrow=nrow(count_vbs),ncol=length(solvers)+1))
      count_vbs_cumulative[1] = count_vbs$InstanceName
      count_vbs_cumulative[2] = count_vbs[,2]
      for(col in 3:ncol(count_vbs)){
        count_vbs_cumulative[col] = as.numeric(count_vbs[,col])+
          as.numeric(count_vbs_cumulative[,col-1])
      }
      count_vbs_cumulative[2:length(solvers)+1] = sapply(count_vbs_cumulative[2:length(solvers)+1] ,as.numeric)
      colnames(count_vbs_cumulative) <- c("InstanceName",c(1:length(solvers)))
      if(saveTo!=""){
        write.csv(count_vbs_cumulative,paste0(saveTo,"/VBS_occurance_test.csv"),
                  row.names = FALSE)
      }
      parallelStop()
      return(count_vbs_cumulative)
    },
    
    #the expected values for prediction, SE, lower and upperbound (mean values across all folds)
    #use only when you have CV
    get_expected_values_validation = function(preds_path_valid,saveTo=""){
      csvs_validation = list.files(preds_path_valid,".csv",full.names = TRUE)
      for(csv in csvs_validation)
      {
        data = read.csv(csv)
        solvers = unique(data$Solver)
        expected_value = data.frame(matrix(ncol=9,nrow = 0))
        probs = c(rep(1,max(unique(data$Iteration))))
        for(solver in solvers){
          data_solver = data[which(data$Solver == solver),]
          expected_pred = sum(data_solver$PredictedRuntime*probs)
          expected_se = sum(data_solver$Prediction_StandardError*probs)
          expected_lowerbound = sum((data_solver$PredictedRuntime - data_solver$Prediction_StandardError)*probs)
          expected_upperbound = sum((data_solver$PredictedRuntime + data_solver$Prediction_StandardError)*probs)
          expected_prediction_rank = sum(data_solver$solver_prediction_rank*probs)
          row = c(unique(data_solver$InstanceName),
                  solver,
                  unique(data_solver$ActualRuntime),
                  expected_pred,
                  expected_se,
                  expected_lowerbound,
                  expected_upperbound,
                  unique(data_solver$Validation_set),
                  expected_prediction_rank)
          expected_value = rbind(expected_value,row)
        }
        colnames(expected_value) = c("InstanceName","Solver","ActualRuntime","ExpectedPrediction","ExpectedSE","ExpectedLowebound","ExpectedUpperbound","Validation_set","ExpectedPredictionRank")
        if(!is.null(saveTo)){
          write.csv(expected_value,paste0(saveTo,"/expectedValues/",tail(str_split(csv,"/")[[1]],1),),row.names = FALSE)
        }
        return(expected_value)
      }
    },
    
    #the expected values for prediction, SE, lower and upperbound for each fold seperatedly 
    #(10 csv file for each instance will be created)
    #use only when you have CV
    get_expected_values_validation_per_fold = function(preds_path_valid){
      csvs_validation = list.files(preds_path_valid,".csv",full.names = TRUE)
      for(csv in csvs_validation)
      {
        data = read.csv(csv)
        for(i in unique(data$Iteration)){
          dt = data[which(data$Iteration == i),]
          solvers = unique(dt$Solver)
          expected_value = data.frame(matrix(ncol=9,nrow = 0))
          probs = c(rep(1,max(unique(dt$Iteration))))
          for(solver in solvers){
            data_solver = dt[which(dt$Solver == solver),]
            expected_pred = sum(data_solver$PredictedRuntime*probs)
            expected_se = sum(data_solver$Prediction_StandardError*probs)
            expected_lowerbound = sum((data_solver$PredictedRuntime - data_solver$Prediction_StandardError)*probs)
            expected_upperbound = sum((data_solver$PredictedRuntime + data_solver$Prediction_StandardError)*probs)
            expected_prediction_rank = sum(data_solver$solver_prediction_rank*probs)
            row = c(unique(data_solver$InstanceName),
                    solver,
                    unique(data_solver$ActualRuntime),
                    expected_pred,
                    expected_se,
                    expected_lowerbound,
                    expected_upperbound,
                    unique(data_solver$Validation_set),
                    expected_prediction_rank)
            expected_value = rbind(expected_value,row)
          }
          colnames(expected_value) = c("InstanceName","Solver","ActualRuntime","ExpectedPrediction","ExpectedSE","ExpectedLowebound","ExpectedUpperbound","Validation_set","ExpectedPredictionRank")
          write.csv(expected_value,paste0(preds_path_valid,"/expectedValues/",i,"_",tail(str_split(csv,"/")[[1]],1),),row.names = FALSE)
        }
      }
    },
    
    #merge all preds into a dataframe
    get_all_preds = function(ignoreInstances = FALSE){
      instances = list.files(paste0(self$predictionPath,"/preds/"),".csv",full.names = TRUE)
      all = do.call(rbind,lapply(instances, read.csv))
      all = all[order(all$Solver),]
      all = all[c(1,2,4)] %>% spread(Solver,PredictedRuntime)
      all = all[order(all$InstanceName),]
      if(ignoreInstances){
        sequential = SequentialPerformance$new()
        sequential$ignore_instances()
        unsolved = sequential$unsolvedInstances
        all = all[which(!all$InstanceName %in%unsolved),]
      }
      return(all)
    },
    
    get_all_SEs = function(ignoreInstances = FALSE){
      instances = list.files(paste0(self$predictionPath,"/preds/"),".csv",full.names = TRUE)
      all = do.call(rbind,lapply(instances, read.csv))
      all = all[order(all$Solver),]
      all = all[c(1,2,5)] %>% spread(Solver,Prediction_StandardError)
      all = all[order(all$InstanceName),]
      if(ignoreInstances){
        sequential = SequentialPerformance$new()
        sequential$ignore_instances()
        unsolved = sequential$unsolvedInstances
        all = all[which(!all$InstanceName %in%unsolved),]
      }
      return(all)
    },

    #plot instances predictions 
    plot_instances_predictions = function(predictionPath = self$predictionPath, savePath = ""){
      instanceCSVs = list.files(predictionPath,pattern = ".csv",full.names = TRUE)
      if(length(instanceCSVs)==0) return("No file was found!")
      parallelStartSocket(cpus = self$cpus)
      for(instanceCSV in instanceCSVs){
        data = read.csv(instanceCSV)
        data$ActualRuntime <- as.numeric(data$ActualRuntime)
        data$PredictedRuntime <- as.numeric(data$PredictedRuntime)
        data$Prediction_StandardError <- as.numeric(data$Prediction_StandardError)
        p<-ggplot(data, aes(x=Solver, y=PredictedRuntime,colour="PredictedRuntime")) +
          geom_point()+
          geom_point(aes(y=ActualRuntime,colour="ActualRuntime"))+
          geom_errorbar(aes(ymin=PredictedRuntime-Prediction_StandardError, 
                            ymax=PredictedRuntime+Prediction_StandardError), width=.2,
                        position=position_dodge(0.05))+
          ggtitle(unique(data$InstanceName))+
          scale_y_continuous(
            name = "Runtime"
            #sec.axis = sec_axis( trans=~./self$Cutoff , name="probability of solving instance RegRF prediction")
          )+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        if(savePath!=""){
          ggsave(paste0(savePath,"/",str_split(str_split(unique(data$InstanceName),".cnf")[[1]][1],
                                               "sat/")[[1]][2],".PNG")
                 ,width = 10,height = 7,dpi = 200)
        }
      }
      
      parallelStop()
      invisible(self)
      return(p)
    },
    
    #single algorithm selection - the mean predicted solver across all folds
    Single_Algorithm_Selection = function(predictionPath= self$predictionPath,savePath="",ignoreTimeoutsOnVBS=FALSE){
      instanceCSVs = list.files(paste0(predictionPath, self$learnertype, "/preds/"),pattern = ".csv",full.names = TRUE)
      if(length(instanceCSVs)==0) return("No file was found!")
      result = data.frame(matrix(nrow = 0,ncol = 8))
      parallelStartSocket(cpus = self$cpus)
      for(instanceCSV in instanceCSVs){
        data = read.csv(instanceCSV)
        #get min prediction row 
        if("ExpectedPrediction" %in% colnames(data)){
          minpred = data[order(data$ExpectedPrediction),][1,]
        }
        else{
          minpred = data[order(data$PredictedRuntime),][1,]
        }
        #get vbs value for the instance
        VBSRuntime = data[order(data$ActualRuntime),][1,"ActualRuntime"]
        #par10 value based on actual value of min predicted solver
        Par10 = if(minpred$ActualRuntime>=self$sequentialData$Cutoff) 
          (self$sequentialData$Cutoff * 10) else minpred$ActualRuntime
        #compute mcp based on actual value of min predicted solver
        MCP = if(minpred$ActualRuntime-VBSRuntime <0) 0 else (minpred$ActualRuntime-VBSRuntime)
        row = cbind(minpred,VBSRuntime,Par10,MCP)
        result = rbind(result,row)
      }
      parallelStop()
      if(ignoreTimeoutsOnVBS){
        result = result[which(result$VBSRuntime<self$sequentialData$Cutoff ),]
      }
      if(savePath!=""){
        write.csv(paste0(savePath,"/SingleAlgorithmSelection.csv"))
      }
      return(result)
    },
    
    #top_selection should be 0 if no preference
    #should be changed based on selection method
    #method = -1 >> no selection just order based on predicted runtime
    #method = 0 >> select top solvers based on mean predicted solver accross all folds
    #method = 1 >> select top solvers based on mean predicted solver accross all folds
    
    #add options for par10 prediction
    selection_based_on_SE = function(predictionPath = self$predictionPath, saveTo = self$selectionPath, method_number = 1, 
                                     top_selection = 0,ignoreTimeoutsOnVBS=FALSE, orderBy = "pred", delta = 0, 
                                     delta_prime = 0, alpha = 0, JP_limit = 0.1, KL_limit = 0.1, 
                                     plot = TRUE){
      print(predictionPath)
      ds.files = list.files(predictionPath,pattern = ".csv")
      solvers = self$sequentialData$solvers
      solvers = solvers[order(solvers)]
      instances = self$sequentialData$instances
      # instances = unlist(lapply(ds.files,function(x){
      #   str_split(x,".csv")[[1]][1]
      # }))
      instances = as.vector(unlist(instances))
      if(ignoreTimeoutsOnVBS) {
        if(is.null(self$sequentialData$solvedInstances)) self$sequentialData$get_VBS()
        # instances = instances[which(paste0("sat/",instances) %in% self$sequentialData$solvedInstances)]
      }
      #ids are  ordered
      
      coresSelection = data.frame(matrix(ncol = 2, nrow = 0))
      parallelStartSocket(cpus = self$cpus)
      for(instance in instances){
        print(instance)
        instanceName = instance
        csv = read.csv(paste0(predictionPath,"/",instanceName,".csv"))
        if(nrow(csv)<1){
          print("predictions are empty!")
        }
        minrow = csv[which(csv$PredictedRuntime == min(csv$PredictedRuntime)),][1,]
        min = minrow$PredictedRuntime
        min_solver = minrow$Solver
        #don't do selection
        if(method_number==-1){
          if(orderBy == "pred-se"){
            print("We cannot consider uncertainty in this method! So, ordering is by prediction value!")
            return()
          }
          if(top_selection != 0){
            print("This method is to run all solvers in parallel, keep the default value for top_selection!")
            return()
          }
          otherRows = csv
          otherRows = otherRows[order(otherRows$PredictedRuntime),]
          data = otherRows
        }
        
        #top selections
        else if(method_number==0){
          if(orderBy == "pred-se"){
            print("We cannot consider uncertainty in this method! So, ordering is by prediction value!")
            return()
          }
          otherRows = csv
          otherRows = otherRows[order(otherRows$PredictedRuntime),]
          if(top_selection != 0 && nrow(otherRows)>top_selection){
            otherRows = otherRows[1:top_selection,]
          }
          data = otherRows
        }
        
        #minpred <= pred <= [minpred + delta_prime*SE]
        else if(method_number==1){
          upperbound = min + (delta_prime*minrow$Prediction_StandardError)
          lowerbound = min
          otherRows = csv[which(csv$PredictedRuntime<= upperbound),]
          otherRows = otherRows[which(otherRows$PredictedRuntime>=lowerbound),]
          #top 5 
          if(orderBy == 'pred'){
            otherRows = otherRows[order(otherRows$PredictedRuntime),]
            if(top_selection != 0 && nrow(otherRows)>top_selection){
              otherRows = otherRows[1:top_selection,]
            }
          } else if(orderBy == 'pred-se'){ 
            otherRows = otherRows[order(otherRows$PredictedRuntime- (alpha*otherRows$Prediction_StandardError)),]
            if(top_selection != 0 && nrow(otherRows)>top_selection){
              otherRows = otherRows[1:top_selection,]
              otherRows = na.omit(otherRows)
              if(!minrow$Solver %in% otherRows$Solver){
                otherRows = rbind(minrow, otherRows)
                if(nrow(otherRows)>top_selection){
                  otherRows = otherRows[1:(nrow(otherRows)-1),]  
                }
              }
            }
          }
          data = otherRows
        }
        
        #lowebound as good as min pred, [pred-delta*SE]<=minpred
        else if(method_number==2){
          upperbound = min
          otherRows = csv[which((csv$PredictedRuntime - (delta*csv$Prediction_StandardError))<= upperbound),]
          if(orderBy == 'pred'){
            otherRows = otherRows[order(otherRows$PredictedRuntime),]
            #top 5 
            if(top_selection!=0){
              if(nrow(otherRows)>top_selection){
                otherRows = otherRows[1:top_selection,]
              }
            }
          } else if(orderBy == 'pred-se'){
            otherRows = otherRows[order(otherRows$PredictedRuntime - (alpha*otherRows$Prediction_StandardError)),]
            if(top_selection!=0){
              if(nrow(otherRows)>top_selection){
                otherRows = otherRows[1:top_selection,]
                otherRows = na.omit(otherRows)
                if(!minrow$Solver %in% otherRows$Solver){
                  otherRows = rbind(minrow, otherRows)
                  if(nrow(otherRows)>top_selection){
                    otherRows = otherRows[1:(nrow(otherRows)-1),]  
                  }
                }
              }
            }  
          }
          data = otherRows
        }
        #generalized 2 and 1
        #p-delta*se <= minP + deltaPrime*SE_min
        else if(method_number==3){
          #p-delta*se <= minP + deltaPrime*SE_min
          upperbound = min + (delta_prime*minrow$Prediction_StandardError)
          otherRows = csv[which((csv$PredictedRuntime - (delta*csv$Prediction_StandardError))<= upperbound),]
          #top 5 
          if(orderBy == 'pred'){
            otherRows = otherRows[order(otherRows$PredictedRuntime),]
            if(top_selection != 0 && nrow(otherRows)>top_selection){
              otherRows = otherRows[1:top_selection,]
            }
          } else if(orderBy == 'pred-se'){ 
            otherRows = otherRows[order(otherRows$PredictedRuntime- (alpha*otherRows$Prediction_StandardError)),]
            if(top_selection != 0 && nrow(otherRows)>top_selection){
              otherRows = otherRows[1:top_selection,]
              otherRows = na.omit(otherRows)
              if(!minrow$Solver %in% otherRows$Solver){
                otherRows = rbind(minrow, otherRows)
                if(nrow(otherRows)>top_selection){
                  otherRows = otherRows[1:(nrow(otherRows)-1),]  
                }
              }
            }
          }
          data = otherRows
        }
        
        #lower as good as min, but nor lower than [minpred-delta_prime*SE], 
        #[minpred-delta_prime*SE]<=[pred-delta*SE]<=minpred
        else if(method_number==4){
          upperbound = min
          lowerbound = min - delta_prime*minrow$Prediction_StandardError
          otherRows = csv[which((csv$PredictedRuntime-delta*csv$Prediction_StandardError)<= upperbound),]
          otherRows = otherRows[which((otherRows$PredictedRuntime-delta*otherRows$Prediction_StandardError)>=lowerbound),]
          if(orderBy == 'pred'){
            otherRows = otherRows[order(otherRows$PredictedRuntime),]
            if(top_selection!=0){
              if(nrow(otherRows)>top_selection){
                otherRows = otherRows[1:top_selection,]
                otherRows = na.omit(otherRows)
                if(!minrow$Solver %in% otherRows$Solver){
                  otherRows = rbind(minrow, otherRows)
                  if(nrow(otherRows)>top_selection){
                    otherRows = otherRows[1:(nrow(otherRows)-1),]  
                  }
                }
              }
            }
          } else if(orderBy == 'pred-se'){
            otherRows = otherRows[order((otherRows$PredictedRuntime-alpha*otherRows$Prediction_StandardError)),]
            if(top_selection!=0){
              if(nrow(otherRows)>top_selection){
                otherRows = otherRows[1:top_selection,]
                otherRows = na.omit(otherRows)
                if(!minrow$Solver %in% otherRows$Solver){
                  otherRows = rbind(minrow, otherRows)
                  if(nrow(otherRows)>top_selection){
                    otherRows = otherRows[1:(nrow(otherRows)-1),]  
                  }
                  
                }
              }
            }
          }
          data = otherRows
        }
        #selection based on Joint probability
        #p_min = 1-pnorm(mean_A-3*sd_A,mean = mean_min,sd = sd_min)
        #p_A = pnorm(mean_min+3*sd_min,mean = mean_A,sd = sd_A)
        #jointprobability = p_min*p_A
        else if(method_number==5){
          mean_min = minrow$PredictedRuntime
          sd_min = minrow$Prediction_StandardError
          otherRows = minrow
          for(row in 1:nrow(csv)){
            mean_A = csv[row,]$PredictedRuntime
            sd_A = csv[row,]$Prediction_StandardError
            c = ((mean_A*(sd_min^2))-(sd_A*((mean_min*sd_A)+sd_min* sqrt((mean_min-mean_A)^2+(2*(sd_min^2-sd_A^2)*log(sd_min/sd_A))))))/(sd_min^2 - sd_A^2)
            ovelapping_area = 1-pnorm(c,mean = mean_min,sd = sd_min)+pnorm(c,mean = mean_A,sd = sd_A)
            # p_min = 1-pnorm(mean_A-3*sd_A,mean = mean_min,sd = sd_min)
            # p_A = pnorm(mean_min+3*sd_min,mean = mean_A,sd = sd_A)
            # jointprobability = p_min*p_A
            # print(sd_min)
            # print("hereeee")
            if(sd_min != 0){
              if(ovelapping_area >= JP_limit || is.na(ovelapping_area)){
                # print(csv[row,])
                if(csv[row,]$Prediction_StandardError == 0){
                  if(csv[row,]$PredictedRuntime != self$sequentialData$Cutoff){
                    if(csv[row,]$Solver != minrow$Solver){
                      point_on_min = qnorm(1 - JP_limit, mean = mean_min, sd = sd_min)
                      if(csv[row,]$PredictedRuntime <= point_on_min && csv[row,]$PredictedRuntime >= mean_min){
                        otherRows = rbind(otherRows, csv[row,])
                      } 
                    }
                  }
                } else{ 
                  if(csv[row,]$Solver != minrow$Solver){
                    otherRows = rbind(otherRows, csv[row,]) 
                  } 
                }           
              }
            }
          }
          if(orderBy == 'pred'){
            otherRows = otherRows[order(otherRows$PredictedRuntime),]
            if(top_selection!=0){
              if(nrow(otherRows)>top_selection){
                otherRows = otherRows[1:top_selection,]
                otherRows = na.omit(otherRows)
                if(!minrow$Solver %in% otherRows$Solver){
                  otherRows = rbind(minrow, otherRows)
                  if(nrow(otherRows)>top_selection){
                    otherRows = otherRows[1:(nrow(otherRows)-1),]  
                  }
                }
              }
            }
          } else if(orderBy == 'pred-se'){
            otherRows = otherRows[order((otherRows$PredictedRuntime-alpha*otherRows$Prediction_StandardError)),]
            if(top_selection!=0){
              if(nrow(otherRows)>top_selection){
                otherRows = otherRows[1:top_selection,]
                otherRows = na.omit(otherRows)
                if(!minrow$Solver %in% otherRows$Solver){
                  otherRows = rbind(minrow, otherRows)
                  if(nrow(otherRows)>top_selection){
                    otherRows = otherRows[1:(nrow(otherRows)-1),]  
                  }
                  
                }
              }
            }
          }
          data = otherRows
        }

        #KL divergence
        else if(method_number==6){
          mean_min = minrow$PredictedRuntime
          sd_min = minrow$Prediction_StandardError
          otherRows = minrow
          for(row in 1:nrow(csv)){
            mean_A = csv[row,]$PredictedRuntime
            sd_A = csv[row,]$Prediction_StandardError
            print(sd_min)
            if(sd_min != 0){
              kl = self$kl_divergence_normal(mean_min, sd_min, mean_A, sd_A)
              if(kl <= KL_limit || is.na(kl)){
                # print(csv[row,])
                if(csv[row,]$Prediction_StandardError == 0){
                  if(csv[row,]$PredictedRuntime != self$sequentialData$Cutoff){
                    if(csv[row,]$Solver != minrow$Solver){
                      # point_on_min = qnorm(1 - KL_limit, mean = mean_min, sd = sd_min)
                      # if(csv[row,]$PredictedRuntime <= point_on_min && csv[row,]$PredictedRuntime >= mean_min){
                        otherRows = rbind(otherRows, csv[row,])
                      # } 
                    }
                  }
                } else{ 
                  if(csv[row,]$Solver != minrow$Solver){
                    otherRows = rbind(otherRows, csv[row,]) 
                  } 
                }           
              }
            }
          }
          
          if(orderBy == 'pred'){
            otherRows = otherRows[order(otherRows$PredictedRuntime),]
            if(top_selection!=0){
              if(nrow(otherRows)>top_selection){
                otherRows = otherRows[1:top_selection,]
                otherRows = na.omit(otherRows)
                if(!minrow$Solver %in% otherRows$Solver){
                  otherRows = rbind(minrow, otherRows)
                  if(nrow(otherRows)>top_selection){
                    otherRows = otherRows[1:(nrow(otherRows)-1),]  
                  }
                }
              }
            }
          } else if(orderBy == 'pred-se'){
            otherRows = otherRows[order((otherRows$PredictedRuntime-alpha*otherRows$Prediction_StandardError)),]
            if(top_selection!=0){
              if(nrow(otherRows)>top_selection){
                otherRows = otherRows[1:top_selection,]
                otherRows = na.omit(otherRows)
                if(!minrow$Solver %in% otherRows$Solver){
                  otherRows = rbind(minrow, otherRows)
                  if(nrow(otherRows)>top_selection){
                    otherRows = otherRows[1:(nrow(otherRows)-1),]  
                  }
                  
                }
              }
            }
          }
          data = otherRows
        }     
        
        #mixture of gaussians , univariate
        else if(method_number==7){
          predictionPath_parallel = sub("/[^/]+/$", "/", sub(self$benchmarks_name, paste0(benchmark_name,"_Parallel"), gsub("/+", "/", predictionPath)))
          instanceName = gsub("([][{}()+*^$.|?])", "\\\\\\1", instanceName, perl=TRUE)
          ins_files = list.files(path = paste0(predictionPath_parallel),
                                  pattern = paste0(".*__", instanceName, "\\.csv$"),
                                  full.names = TRUE)
          csvs = lapply(ins_files, read.csv)
          csvs = do.call(rbind, csvs)
          x_values = seq(0, self$sequentialData$Cutoff, length.out = 10000)
          
          # Split the combined dataframe by Solver
          solver_dfs = split(csvs, csvs$Solver)

          min_solver_df = solver_dfs[[minrow$Solver]]
          min_solver_df[min_solver_df$Prediction_StandardError < 10^-5, "Prediction_StandardError"] <- 0
          portfolio = vector()
          portfolio = append(portfolio, minrow$Solver)
          portfolio_means = vector()
          
          if(mean(min_solver_df$PredictedRuntime == self$sequentialData$Cutoff)){
            mean_min = self$sequentialData$Cutoff  
          } else{
            mean_min = self$find_quantile_mixture(0.5, mean = min_solver_df$PredictedRuntime, sd = min_solver_df$Prediction_StandardError)
          }
          portfolio_means = append(portfolio_means, mean_min)
          
          # print(min_solver_df)
          # Initialize an empty list to store data frames for plotting
          plot_data_list <- list()
          
          for(i in seq_along(solver_dfs)){
            solver <- names(solver_dfs)[i]
            solver_df <- solver_dfs[[solver]]
            solver_df[solver_df$Prediction_StandardError < 10^-5, "Prediction_StandardError"] <- 0
            # print(solver_df)
            if(plot){ 
                # Calculate the mixture PDF for this solver
                pdf_values <- self$mixture_pdf(x_values, solver_df$PredictedRuntime, solver_df$Prediction_StandardError)
                
                # Create a data frame for this solver's plot get_gaussian_mixture_jp_optimal_selections
                df <- data.frame(x = x_values, y = pdf_values, Solver = solver)
                
                # Add this data frame to the list
                plot_data_list[[i]] <- df
            }
            if((mean(min_solver_df$PredictedRuntime == self$sequentialData$Cutoff)<1) && (mean(min_solver_df$Prediction_StandardError)!=0)){
              if(solver != min_solver && mean(solver_df$PredictedRuntime == self$sequentialData$Cutoff)<1){
                sign_changes_points = self$find_sign_changes(means1 = min_solver_df$PredictedRuntime, sds1 = min_solver_df$Prediction_StandardError, means2 = solver_df$PredictedRuntime, sds2 = solver_df$Prediction_StandardError, from = 0, to = self$sequentialData$Cutoff, steps = 10000)
                c = vector()
                for(point_of_interest in sign_changes_points){
                  small_interval <- c(point_of_interest - 0.5, point_of_interest + 0.5)
                  # Find the actual roots
                  p = self$find_roots(small_interval, means1 = min_solver_df$PredictedRuntime, sds1 = min_solver_df$Prediction_StandardError, means2 = solver_df$PredictedRuntime, sds2 = solver_df$Prediction_StandardError)
                  c = append(c,p[which(p>0)])
                }
                c = min(c)
                if(!is.infinite(c)){
                  #2 khate movazi nabashan 
                  #area
                  ovelapping_area = 1 - self$cdf_mixture(c, means = min_solver_df$PredictedRuntime, sds = min_solver_df$Prediction_StandardError) + self$cdf_mixture(c, means = solver_df$PredictedRuntime, sds = solver_df$Prediction_StandardError)
                  #if(sd_min != 0){
                  if(ovelapping_area >= JP_limit || is.na(ovelapping_area)){
                    # if overlaparea is na >> the SE is zero
                    # not sure how to handle it here 
                    if(is.na(ovelapping_area)){
                      point_on_min = self$find_quantile_mixture(1 - JP_limit, mean = min_solver_df$PredictedRuntime, sd = min_solver_df$Prediction_StandardError)
                      mean_min = self$find_quantile_mixture(0.5, mean = min_solver_df$PredictedRuntime, sd = min_solver_df$Prediction_StandardError)
                      mean_solver = self$find_quantile_mixture(0.5, mean = solver_df$PredictedRuntime, sd = solver_df$Prediction_StandardError)
                      if(mean_solver <= point_on_min && mean_solver >= mean_min){
                        portfolio = append(portfolio, solver)
                        portfolio_means = append(portfolio_means, mean_solver)
                      } 
                    } else{
                      mean_solver = self$find_quantile_mixture(0.5, mean = solver_df$PredictedRuntime, sd = solver_df$Prediction_StandardError)
                      portfolio = append(portfolio, solver)
                      portfolio_means = append(portfolio_means, mean_solver)
                    }
                  } 
                }
              }
            }
          }
          print("portfolio:")
          print(portfolio)
          if(plot){
            pdf_values <- self$mixture_pdf(x_values, min_solver_df$PredictedRuntime, min_solver_df$Prediction_StandardError)
            
            # Create a data frame for this solver's plot data
            df <- data.frame(x = x_values, y = pdf_values, Solver = min_solver)
                
            # Add this data frame to the list
            plot_data_list[[i]] <- df

            # Combine all solvers' data frames into one
            plot_data <- do.call(rbind, plot_data_list)
                        # Plot using ggplot
            p <- ggplot(plot_data, aes(x = x, y = y, color = Solver)) +
              geom_line() +
              theme_minimal() +
              ggtitle('PDF of Mixture of Gaussians') +
              xlab('x') +
              ylab('Density')

            # Display the plot
            print(p)

            # Save the plot to a file
            ggsave(paste0(predictionPath_parallel,"/../gaussian_mixture_equal_weigth/", "gaussian_mixture_equal_weigth_",instanceName,".pdf"), plot = p, width = 10, height = 8, dpi = 300)
          }
          portfolio_df = data.frame(matrix(nrow = length(portfolio), ncol = 0))
          portfolio_df$Solver = portfolio
          portfolio_df$Mean_GaussianMixture = portfolio_means
          data = csv[which(csv$Solver %in% portfolio_df$Solver),]
          # Assuming 'SolverData' in 'data' df and 'SolverPortfolio' in 'portfolio_df' df are the columns to merge on
          data = merge(data, portfolio_df, by = "Solver", )
          data = data[order(data$Mean_GaussianMixture),]
          if(top_selection!=0 && nrow(data)> top_selection){
            data = data[c(1:top_selection),]
          }
          # data$InstanceName = paste0(data$InstanceName)
          data = data[c("InstanceName","Solver","ActualRuntime","PredictedRuntime","Prediction_StandardError")]
        }

        data = data[1:5]
        colnames(data)<-c("InstanceName","Solver","ActualRuntime","PredictedRuntime","Prediction_StandardError")
        number_of_solvers = nrow(data)
        print(number_of_solvers)
        # if(number_of_solvers == 1){
        #   print(csv)
        # }
        actual_parallel_runtime = data.frame()
        path = self$sequentialData$actual_CSV_path
        path = str_split(path,"/")[[1]]
        path = path[-length(path)]
        path = paste0(path, collapse = '/') 
        #this can be improved by a reg model 
        if(number_of_solvers==1){
          filename = paste0(path,"/teton-",self$benchmarks_name,"-",length(self$sequentialData$solvers),"-solvers-solo.csv")
          actual_parallel_runtime = read.csv(filename)
        } else if (number_of_solvers<10){
          filename = paste0(path,"/teton-",self$benchmarks_name,"-",length(self$sequentialData$solvers),"-solvers-",number_of_solvers,"-parallel.csv")
          actual_parallel_runtime = read.csv(filename)
        } else if(number_of_solvers>31){
          filename = paste0(path,"/teton-",self$benchmarks_name,"-",length(self$sequentialData$solvers),"-solvers-32-parallel.csv")
          actual_parallel_runtime = read.csv(filename)    
        } else if(number_of_solvers>=10 ){
          core = round(number_of_solvers,-1)
          filename = paste0(path,"/teton-",self$benchmarks_name,"-",length(self$sequentialData$solvers),"-solvers-",core,"-parallel.csv")
          actual_parallel_runtime = read.csv(filename)
        } 
        parallel_runtimes <- vector()
        csvrow = actual_parallel_runtime[which(actual_parallel_runtime$InstanceName == data[1,]$InstanceName),]
        if(nrow(csvrow)==0){
          substr = strsplit(data[1,]$InstanceName, "__")[[1]][2]
          matching_rows = grep(substr, actual_parallel_runtime$InstanceName)
          if (length(matching_rows) == 1) {
            csvrow <- actual_parallel_runtime[matching_rows,]
          } else {
            # Handle the case where no matches are found
            print(paste0("No match found for",  actual_parallel_runtime$InstanceName))
            csvrow <- NULL  # or some other default value or action
          }
        }
        for(m in 0:nrow(data)){
          parallel_runtimes <- append(parallel_runtimes,csvrow[data[m,]$Solver])
          parallel_runtimes = unname(unlist(parallel_runtimes))
        }
        vbs_runtime = self$sequentialData$get_VBS()[which(self$sequentialData$get_VBS()$InstanceName == data[1,]$InstanceName),]$VBS_Runtime
        #parallel_runtimes[which(parallel_runtimes==5000)]<-50000
        #vbs_runtime[which(vbs_runtime==5000)]<-50000
        data <- cbind(data,parallel_runtimes)
        data <- cbind(data,c(rep(vbs_runtime,number_of_solvers)))
        #colnames(data) = c("instance","solver","sequential_par10","prediction","standardError","parallel_par10","vbs_par10")
        colnames(data) = c("InstanceName","Solver","SequentialRuntime","PredictedRuntime",
                           "Prediction_StandardError","ParallelRuntime","VBSRuntime")
        data$delta = rep(x = delta, times = nrow(data))
        data$delta_prime = rep(x = delta_prime, times = nrow(data))
        data$alpha = rep(x = alpha, times = nrow(data))
        data$method_number = method_number
        data$orderBy = orderBy
        coresSelection = rbind(coresSelection,c(instance,nrow(data)))
        colnames(coresSelection) <- c("instances","selectedCores")
        
        if(saveTo!="")
          write.csv(data,paste0(saveTo,"/",instanceName,".csv"),row.names = FALSE)
      }
      parallelStop()
      return(coresSelection)
    },
    
    selection_based_on_SE_parallelruns = function(predictionPath = self$predictionPath, saveTo = self$selectionPath, method_number = 1, 
                                                  top_selection = 0,ignoreTimeoutsOnVBS=FALSE, orderBy = "pred", delta = 0, 
                                                  delta_prime = 0, alpha = 0, JP_limit = 0.1, KL_limit = 0.1, weights=NULL, plot = TRUE){
      print(predictionPath)
      ds.files = list.files(predictionPath,pattern = ".csv")
      solvers = self$sequentialData$solvers
      solvers = solvers[order(solvers)]
      instances = self$sequentialData$instances
      # instances = unlist(lapply(ds.files,function(x){
      #   str_split(x,".csv")[[1]][1]
      # }))
      instances = as.vector(unlist(instances))
      if(ignoreTimeoutsOnVBS) {
        if(is.null(self$sequentialData$solvedInstances)) self$sequentialData$get_VBS()
        instances = instances[which(paste0("sat/",instances) %in% self$sequentialData$solvedInstances)]
      }
      #ids are  ordered
      coresSelection = data.frame(matrix(ncol = 2, nrow = 0))
      parallelStartSocket(cpus = self$cpus)
      for(instance in instances){
        instanceName = instance
        csv = read.csv(paste0(predictionPath,"/",instanceName,".csv"))
        if(nrow(csv)<1){
          print("predictions are empty!")
        }
        minrow = csv[which(csv$PredictedRuntime == min(csv$PredictedRuntime)),][1,]
        min = minrow$PredictedRuntime
        #don't do selection
        if(method_number==-1){
          if(orderBy == "pred-se"){
            print("We cannot consider uncertainty in this method! So, ordering is by prediction value!")
            return()
          }
          if(top_selection != 0){
            print("This method is to run all solvers in parallel, keep the default value for top_selection!")
            return()
          }
          otherRows = csv
          otherRows = otherRows[order(otherRows$PredictedRuntime),]
          data = otherRows
        }
        
        #top selections
        else if(method_number==0){
          if(orderBy == "pred-se"){
            print("We cannot consider uncertainty in this method! So, ordering is by prediction value!")
            return()
          }
          otherRows = csv
          otherRows = otherRows[order(otherRows$PredictedRuntime),]
          if(top_selection != 0 && nrow(otherRows)>top_selection){
            otherRows = otherRows[1:top_selection,]
          }
          data = otherRows
        }
        
        #minpred <= pred <= [minpred + delta_prime*SE]
        else if(method_number==1){
          upperbound = min + (delta_prime*minrow$Prediction_StandardError)
          lowerbound = min
          otherRows = csv[which(csv$PredictedRuntime<= upperbound),]
          otherRows = otherRows[which(otherRows$PredictedRuntime>=lowerbound),]
          #top 5 
          if(orderBy == 'pred'){
            otherRows = otherRows[order(otherRows$PredictedRuntime),]
            if(top_selection != 0 && nrow(otherRows)>top_selection){
              otherRows = otherRows[1:top_selection,]
            }
          } else if(orderBy == 'pred-se'){ 
            otherRows = otherRows[order(otherRows$PredictedRuntime- (alpha*otherRows$Prediction_StandardError)),]
            if(top_selection != 0 && nrow(otherRows)>top_selection){
              otherRows = otherRows[1:top_selection,]
              otherRows = na.omit(otherRows)
              if(!minrow$Solver %in% otherRows$Solver){
                otherRows = rbind(minrow, otherRows)
                if(nrow(otherRows)>top_selection){
                  otherRows = otherRows[1:(nrow(otherRows)-1),]  
                }
              }
            }
          }
          data = otherRows
        }
        
        #lowebound as good as min pred, [pred-delta*SE]<=minpred
        else if(method_number==2){
          upperbound = min
          otherRows = csv[which((csv$PredictedRuntime - (delta*csv$Prediction_StandardError))<= upperbound),]
          if(orderBy == 'pred'){
            otherRows = otherRows[order(otherRows$PredictedRuntime),]
            #top 5 
            if(top_selection!=0){
              if(nrow(otherRows)>top_selection){
                otherRows = otherRows[1:top_selection,]
              }
            }
          } else if(orderBy == 'pred-se'){
            otherRows = otherRows[order(otherRows$PredictedRuntime - (alpha*otherRows$Prediction_StandardError)),]
            if(top_selection!=0){
              if(nrow(otherRows)>top_selection){
                otherRows = otherRows[1:top_selection,]
                otherRows = na.omit(otherRows)
                if(!minrow$Solver %in% otherRows$Solver){
                  otherRows = rbind(minrow, otherRows)
                  if(nrow(otherRows)>top_selection){
                    otherRows = otherRows[1:(nrow(otherRows)-1),]  
                  }
                }
              }
            }  
          }
          data = otherRows
        }
        #generalized 2 and 1
        #p-delta*se <= minP + deltaPrime*SE_min
        else if(method_number==3){
          #p-delta*se <= minP + deltaPrime*SE_min
          upperbound = min + (delta_prime*minrow$Prediction_StandardError)
          otherRows = csv[which((csv$PredictedRuntime - (delta*csv$Prediction_StandardError))<= upperbound),]
          #top 5 
          if(orderBy == 'pred'){
            otherRows = otherRows[order(otherRows$PredictedRuntime),]
            if(top_selection != 0 && nrow(otherRows)>top_selection){
              otherRows = otherRows[1:top_selection,]
            }
          } else if(orderBy == 'pred-se'){ 
            otherRows = otherRows[order(otherRows$PredictedRuntime- (alpha*otherRows$Prediction_StandardError)),]
            if(top_selection != 0 && nrow(otherRows)>top_selection){
              otherRows = otherRows[1:top_selection,]
              otherRows = na.omit(otherRows)
              if(!minrow$Solver %in% otherRows$Solver){
                otherRows = rbind(minrow, otherRows)
                if(nrow(otherRows)>top_selection){
                  otherRows = otherRows[1:(nrow(otherRows)-1),]  
                }
              }
            }
          }
          data = otherRows
        }
        
        #lower as good as min, but nor lower than [minpred-delta_prime*SE], 
        #[minpred-delta_prime*SE]<=[pred-delta*SE]<=minpred
        else if(method_number==4){
          upperbound = min
          lowerbound = min - delta_prime*minrow$Prediction_StandardError
          otherRows = csv[which((csv$PredictedRuntime-delta*csv$Prediction_StandardError)<= upperbound),]
          otherRows = otherRows[which((otherRows$PredictedRuntime-delta*otherRows$Prediction_StandardError)>=lowerbound),]
          if(orderBy == 'pred'){
            otherRows = otherRows[order(otherRows$PredictedRuntime),]
            if(top_selection!=0){
              if(nrow(otherRows)>top_selection){
                otherRows = otherRows[1:top_selection,]
                otherRows = na.omit(otherRows)
                if(!minrow$Solver %in% otherRows$Solver){
                  otherRows = rbind(minrow, otherRows)
                  if(nrow(otherRows)>top_selection){
                    otherRows = otherRows[1:(nrow(otherRows)-1),]  
                  }
                }
              }
            }
          } else if(orderBy == 'pred-se'){
            otherRows = otherRows[order((otherRows$PredictedRuntime-alpha*otherRows$Prediction_StandardError)),]
            if(top_selection!=0){
              if(nrow(otherRows)>top_selection){
                otherRows = otherRows[1:top_selection,]
                otherRows = na.omit(otherRows)
                if(!minrow$Solver %in% otherRows$Solver){
                  otherRows = rbind(minrow, otherRows)
                  if(nrow(otherRows)>top_selection){
                    otherRows = otherRows[1:(nrow(otherRows)-1),]  
                  }
                  
                }
              }
            }
          }
          data = otherRows
        }
        #selection based on Joint probability
        #p_min = 1-pnorm(mean_A-3*sd_A,mean = mean_min,sd = sd_min)
        #p_A = pnorm(mean_min+3*sd_min,mean = mean_A,sd = sd_A)
        #jointprobability = p_min*p_A
        else if(method_number==5){
          mean_min = minrow$PredictedRuntime
          sd_min = minrow$Prediction_StandardError
          otherRows = minrow
          for(row in 1:nrow(csv)){
            mean_A = csv[row,]$PredictedRuntime
            sd_A = csv[row,]$Prediction_StandardError
            c = ((mean_A*(sd_min^2))-(sd_A*((mean_min*sd_A)+sd_min* sqrt((mean_min-mean_A)^2+(2*(sd_min^2-sd_A^2)*log(sd_min/sd_A))))))/(sd_min^2 - sd_A^2)
            ovelapping_area = 1-pnorm(c,mean = mean_min,sd = sd_min)+pnorm(c,mean = mean_A,sd = sd_A)
            # p_min = 1-pnorm(mean_A-3*sd_A,mean = mean_min,sd = sd_min)
            # p_A = pnorm(mean_min+3*sd_min,mean = mean_A,sd = sd_A)
            # jointprobability = p_min*p_A
            print(sd_min)
            if(sd_min != 0){
              if(ovelapping_area >= JP_limit || is.na(ovelapping_area)){
                # print(csv[row,])
                if(csv[row,]$Prediction_StandardError == 0){
                  if(csv[row,]$PredictedRuntime != self$sequentialData$Cutoff){
                    if(csv[row,]$Solver != minrow$Solver){
                      point_on_min = qnorm(1 - JP_limit, mean = mean_min, sd = sd_min)
                      if(csv[row,]$PredictedRuntime <= point_on_min && csv[row,]$PredictedRuntime >= mean_min){
                        otherRows = rbind(otherRows, csv[row,])
                      } 
                    }
                  }
                } else{ 
                  if(csv[row,]$Solver != minrow$Solver){
                    otherRows = rbind(otherRows, csv[row,]) 
                  } 
                }           
              }
            }
          }
          if(orderBy == 'pred'){
            otherRows = otherRows[order(otherRows$PredictedRuntime),]
            if(top_selection!=0){
              if(nrow(otherRows)>top_selection){
                otherRows = otherRows[1:top_selection,]
                otherRows = na.omit(otherRows)
                if(!minrow$Solver %in% otherRows$Solver){
                  otherRows = rbind(minrow, otherRows)
                  if(nrow(otherRows)>top_selection){
                    otherRows = otherRows[1:(nrow(otherRows)-1),]  
                  }
                }
              }
            }
          } else if(orderBy == 'pred-se'){
            otherRows = otherRows[order((otherRows$PredictedRuntime-alpha*otherRows$Prediction_StandardError)),]
            if(top_selection!=0){
              if(nrow(otherRows)>top_selection){
                otherRows = otherRows[1:top_selection,]
                otherRows = na.omit(otherRows)
                if(!minrow$Solver %in% otherRows$Solver){
                  otherRows = rbind(minrow, otherRows)
                  if(nrow(otherRows)>top_selection){
                    otherRows = otherRows[1:(nrow(otherRows)-1),]  
                  }
                  
                }
              }
            }
          }
          data = otherRows
        }
        
        #KL divergence
        else if(method_number==6){
          mean_min = minrow$PredictedRuntime
          sd_min = minrow$Prediction_StandardError
          otherRows = minrow
          for(row in 1:nrow(csv)){
            mean_A = csv[row,]$PredictedRuntime
            sd_A = csv[row,]$Prediction_StandardError
            kl = self$kl_divergence_normal(mean_min, sd_min, mean_A, sd_A)
            print(sd_min)
            if(sd_min != 0){
              if(kl <= KL_limit || is.na(kl)){
                # print(csv[row,])
                if(csv[row,]$Prediction_StandardError == 0){
                  if(csv[row,]$PredictedRuntime != self$sequentialData$Cutoff){
                    if(csv[row,]$Solver != minrow$Solver){
                      # point_on_min = qnorm(1 - KL_limit, mean = mean_min, sd = sd_min)
                      # if(csv[row,]$PredictedRuntime <= point_on_min && csv[row,]$PredictedRuntime >= mean_min){
                        otherRows = rbind(otherRows, csv[row,])
                      # } 
                    }
                  }
                } else{ 
                  if(csv[row,]$Solver != minrow$Solver){
                    otherRows = rbind(otherRows, csv[row,]) 
                  } 
                }           
              }
            }
          }

          if(orderBy == 'pred'){
            otherRows = otherRows[order(otherRows$PredictedRuntime),]
            if(top_selection!=0){
              if(nrow(otherRows)>top_selection){
                otherRows = otherRows[1:top_selection,]
                otherRows = na.omit(otherRows)
                if(!minrow$Solver %in% otherRows$Solver){
                  otherRows = rbind(minrow, otherRows)
                  if(nrow(otherRows)>top_selection){
                    otherRows = otherRows[1:(nrow(otherRows)-1),]  
                  }
                }
              }
            }
          } else if(orderBy == 'pred-se'){
            otherRows = otherRows[order((otherRows$PredictedRuntime-alpha*otherRows$Prediction_StandardError)),]
            if(top_selection!=0){
              if(nrow(otherRows)>top_selection){
                otherRows = otherRows[1:top_selection,]
                otherRows = na.omit(otherRows)
                if(!minrow$Solver %in% otherRows$Solver){
                  otherRows = rbind(minrow, otherRows)
                  if(nrow(otherRows)>top_selection){
                    otherRows = otherRows[1:(nrow(otherRows)-1),]  
                  }
                  
                }
              }
            }
          }
          data = otherRows
        }
        
        data = data[1:5]
        colnames(data)<-c("InstanceName","Solver","ActualRuntime","PredictedRuntime","Prediction_StandardError")
        number_of_solvers = nrow(data)
        actual_parallel_runtime = data.frame()
        path = self$sequentialData$actual_CSV_path
        path = str_split(path,"/")[[1]]
        path = path[-length(path)]
        path = paste0(path, collapse = '/') 
        #this can be improved by a reg model 
        
        
        parallelruns = as.numeric(strsplit(gsub("sat/","",data[1,]$InstanceName), "__")[[1]][1])
        if(number_of_solvers==1){
          filename = paste0(path,"/teton-",self$benchmarks_name,"-",length(self$sequentialData$solvers),"-solvers-solo.csv")
          actual_parallel_runtime = read.csv(filename)
        } else if (number_of_solvers<10){
          filename = paste0(path,"/teton-",self$benchmarks_name,"-",length(self$sequentialData$solvers),"-solvers-",number_of_solvers,"-parallel.csv")
          actual_parallel_runtime = read.csv(filename)
        } else if(number_of_solvers>31){
          filename = paste0(path,"/teton-",self$benchmarks_name,"-",length(self$sequentialData$solvers),"-solvers-32-parallel.csv")
          actual_parallel_runtime = read.csv(filename)    
        } else if(number_of_solvers>=10 ){
          core = round(number_of_solvers,-1)
          filename = paste0(path,"/teton-",self$benchmarks_name,"-",length(self$sequentialData$solvers),"-solvers-",core,"-parallel.csv")
          actual_parallel_runtime = read.csv(filename)
        } 
        parallel_runtimes <- vector()
        split_string <- strsplit(data[1,]$InstanceName, "__")[[1]]
        sub_string <- split_string[2:length(split_string)]
        subins <- paste0(sub_string, collapse = "__")
        
        #csvrow = actual_parallel_runtime[which(str_detect(actual_parallel_runtime$InstanceName, paste0(number_of_solvers,"__",subins))),]
        if(self$benchmarks_name == "SAT2016_Parallel" || self$benchmarks_name == "SAT2018_Parallel"){
          csvrow = actual_parallel_runtime[which(actual_parallel_runtime$InstanceName == paste0("sat/",number_of_solvers,"__",subins)),]
        } else{
          csvrow = actual_parallel_runtime[which(actual_parallel_runtime$InstanceName == paste0(number_of_solvers,"__",subins)),]
        }
        
        for(m in 0:nrow(data)){
          parallel_runtimes <- append(parallel_runtimes,csvrow[data[m,]$Solver])
          parallel_runtimes = unname(unlist(parallel_runtimes))
        }
        
        if(self$benchmarks_name == "SAT2016_Parallel" || self$benchmarks_name == "SAT2018_Parallel"){
          vbs_runtime = self$sequentialData$get_VBS()[which(self$sequentialData$get_VBS()$InstanceName == paste0("sat/",1,"__",subins)),]$VBS_Runtime
        } else{
          vbs_runtime = self$sequentialData$get_VBS()[which(self$sequentialData$get_VBS()$InstanceName == paste0(1,"__",subins)),]$VBS_Runtime
        }
        #parallel_runtimes[which(parallel_runtimes==5000)]<-50000
        #vbs_runtime[which(vbs_runtime==5000)]<-50000
        data <- cbind(data,parallel_runtimes)
        data <- cbind(data,c(rep(vbs_runtime,number_of_solvers)))
        #colnames(data) = c("instance","solver","sequential_par10","prediction","standardError","parallel_par10","vbs_par10")
        colnames(data) = c("InstanceName","Solver","SequentialRuntime","PredictedRuntime",
                           "Prediction_StandardError","ParallelRuntime","VBSRuntime")
        data$delta = rep(x = delta, times = nrow(data))
        data$delta_prime = rep(x = delta_prime, times = nrow(data))
        data$alpha = rep(x = alpha, times = nrow(data))
        data$method_number = method_number
        data$orderBy = orderBy
        coresSelection = rbind(coresSelection,c(instance,nrow(data)))
        colnames(coresSelection) <- c("instances","selectedCores")
        
        if(saveTo!="")
          tools = Tools$new()$create_directory_if_not_exists(paste0(saveTo,"/",parallelruns,"_parallelrun/"))
          write.csv(data,paste0(saveTo,"/",parallelruns,"_parallelrun/",instanceName,".csv"),row.names = FALSE)
      }
      parallelStop()
      return(coresSelection)
    },
    
    #ned to have csvs from (selection-based_on_SE)
    time_splitting_scheduling = function(predictionPath = self$predictionPath, selectionPath = self$selectionPath, cores = 1,
                                         ignoreTimeoutsOnVBS=FALSE, orderBy = "pred", a = 1){
      instances = self$sequentialData$actual_CSV$InstanceName
      if(self$benchmarks_name %in% c("SAT2018","SAT2016")){
        instances = unlist(lapply(instances,function(x){
          str_split(x,"/")[[1]][2]
        }))
      }
      for(instance in instances){
        preds = read.csv(paste0(predictionPath,instance,".csv"))
        
        if(orderBy == "pred+SE"){
          preds = preds[order(preds$PredictedRuntime + preds$Prediction_StandardError),]
        } else if(orderBy == "pred+aSE"){
          preds = preds[order(preds$PredictedRuntime + a * preds$Prediction_StandardError),]
        } else {
          preds = preds[order(preds$PredictedRuntime),]
        }
        
        actual_parallel_runtime = data.frame()
        path = self$sequentialData$actual_CSV_path
        path = str_split(path,"/")[[1]]
        path = path[-length(path)]
        path = paste0(path, collapse = '/') 
        #this can be improved by a reg model 
        if(cores==1){
          filename = paste0(path,"/teton-",self$benchmarks_name,"-",length(self$sequentialData$solvers),"-solvers-solo.csv")
          actual_parallel_runtime = read.csv(filename)
        } else if (cores<10){
          filename = paste0(path,"/teton-",self$benchmarks_name,"-",length(self$sequentialData$solvers),"-solvers-",cores,"-parallel.csv")
          actual_parallel_runtime = read.csv(filename)
        } else if(cores>31){
          filename = paste0(path,"/teton-",self$benchmarks_name,"-",length(self$sequentialData$solvers),"-solvers-32-parallel.csv")
          actual_parallel_runtime = read.csv(filename)    
        } else if(cores>=10 ){
          core = round(cores,-1)
          filename = paste0(path,"/teton-",self$benchmarks_name,"-",length(self$sequentialData$solvers),"-solvers-",core,"-parallel.csv")
          actual_parallel_runtime = read.csv(filename)
        } 
        parallel_runtimes <- vector()
        csvrow = actual_parallel_runtime[which(actual_parallel_runtime$InstanceName == preds[1,]$InstanceName),]
        for(m in 0:nrow(preds)){
          parallel_runtimes <- append(parallel_runtimes,csvrow[preds[m,]$Solver])
          parallel_runtimes = unname(unlist(parallel_runtimes))
        }
        vbs_runtime = self$sequentialData$get_VBS()[which(self$sequentialData$get_VBS()$InstanceName == preds[1,]$InstanceName),]$VBS_Runtime
        #parallel_runtimes[which(parallel_runtimes==5000)]<-50000
        #vbs_runtime[which(vbs_runtime==5000)]<-50000
        preds <- cbind(preds,parallel_runtimes)
        preds <- cbind(preds,c(rep(vbs_runtime,nrow(preds))))
        colnames(preds) = c("InstanceName","Solver","SequentialRuntime","PredictedRuntime",
                            "Prediction_StandardError","ParallelRuntime","VBSRuntime")
        
        coreCol = rep(c(1:cores),nrow(preds)/cores)
        if(length(coreCol)<nrow(preds)){
          for(c in 1:(nrow(preds)-length(coreCol))){
            coreCol = append(coreCol, c)
          }
        }
        preds$cores = coreCol
        
        solvedTime = data.frame(matrix(nrow = 0, ncol = 2))
        elem = 1
        for(c in 1:cores){
          solvers = preds[which(preds$cores == c),]$Solver
          total = 0
          for(s in 1:length(solvers)){
            solver_time = preds[which(preds$Solver == solvers[s]),] 
            if(orderBy == "pred"){
              scheduled_for = solver_time$PredictedRuntime
            } else if(orderBy == "pred+SE") {
              scheduled_for = solver_time$PredictedRuntime + solver_time$Prediction_StandardError
            } else if(orderBy == "pred+aSE") {
              scheduled_for = solver_time$PredictedRuntime + a*solver_time$Prediction_StandardError
            } else {
              scheduled_for = self$sequentialData$Cutoff
            }
            if(scheduled_for > self$sequentialData$Cutoff){
              scheduled_for = self$sequentialData$Cutoff
            }
            solver_time = solver_time$ParallelRuntime
            solvedFlag = FALSE
            if(solver_time <= scheduled_for && solver_time <= (self$sequentialData$Cutoff - total)){
              solvedFlag = TRUE
              total = total + solver_time                  
              solvedTime = rbind(solvedTime,c(solvers[s],total))
              elem = elem +1
              if(s < length(solvers)){
                for(r in (s+1):length(solvers)){
                  solvedTime= rbind(solvedTime,c(solvers[r],Inf))
                  elem = elem +1
                }
              }
              break
            } else {
              total = total + scheduled_for
              solvedTime= rbind(solvedTime,c(solvers[s],Inf))
            }
            s = s+1
          }
        }
        colnames(solvedTime) <- c("Solver","ResultingTime")
        colnam = colnames(preds)
        preds = merge(solvedTime,preds,x.by=Solver)[c(colnam, "ResultingTime")]
        if(orderBy == "pred"){
          preds = preds[order(preds$PredictedRuntime),]
        } else if(orderBy == "pred+SE") {
          preds = preds[order(preds$PredictedRuntime + preds$Prediction_StandardError),]
        } else if(orderBy == "pred+aSE"){
          preds = preds[order(preds$PredictedRuntime + a* preds$Prediction_StandardError),]
        }
        
        write.csv(preds,paste0(selectionPath,"/",instance,".csv"),row.names = FALSE)
      }
    },
    
    
    #ned to have csvs from (selection-based_on_SE)
    time_splitting_scheduling_parallel = function(predictionPath = self$predictionPath, selectionPath = self$selectionPath, cores = 1,
                                         ignoreTimeoutsOnVBS=FALSE, orderBy = "pred", a = 1){
      instances = self$sequentialData$actual_CSV$InstanceName
      if(self$benchmarks_name %in% c("SAT2018_Parallel","SAT2016_Parallel")){
        instances = instances[which(startsWith(instances, paste0("sat/",cores,"__")))]
        instances = unlist(lapply(instances,function(x){
          str_split(x,"/")[[1]][2]
        }))
      } else {
        instances = instances[which(startsWith(instances, paste0(cores,"__")))]
      }
      for(instance in instances){
        preds = read.csv(paste0(predictionPath,instance,".csv"))
        
        if(orderBy == "pred+SE"){
          preds = preds[order(preds$PredictedRuntime + preds$Prediction_StandardError),]
        } else if(orderBy == "pred+aSE"){
          preds = preds[order(preds$PredictedRuntime + a * preds$Prediction_StandardError),]
        } else {
          preds = preds[order(preds$PredictedRuntime),]
        }
        
        actual_parallel_runtime = data.frame()
        path = self$sequentialData$actual_CSV_path
        path = str_split(path,"/")[[1]]
        path = path[-length(path)]
        path = paste0(path, collapse = '/') 
        #this can be improved by a reg model 
        
        if(cores==1){
          filename = paste0(path,"/teton-",self$benchmarks_name,"-",length(self$sequentialData$solvers),"-solvers-solo.csv")
          actual_parallel_runtime = read.csv(filename)
        } else if (cores<10){
          filename = paste0(path,"/teton-",self$benchmarks_name,"-",length(self$sequentialData$solvers),"-solvers-",cores,"-parallel.csv")
          actual_parallel_runtime = read.csv(filename)
        } else if(cores>31){
          filename = paste0(path,"/teton-",self$benchmarks_name,"-",length(self$sequentialData$solvers),"-solvers-32-parallel.csv")
          actual_parallel_runtime = read.csv(filename)    
        } else if(cores>=10 ){
          core = round(cores,-1)
          filename = paste0(path,"/teton-",self$benchmarks_name,"-",length(self$sequentialData$solvers),"-solvers-",core,"-parallel.csv")
          actual_parallel_runtime = read.csv(filename)
        } 
        parallel_runtimes <- vector()
        
        csvrow = actual_parallel_runtime[which(actual_parallel_runtime$InstanceName == preds[1,]$InstanceName),]
        for(m in 0:nrow(preds)){
          parallel_runtimes <- append(parallel_runtimes,csvrow[preds[m,]$Solver])
          parallel_runtimes = unname(unlist(parallel_runtimes))
        }
        vbs_runtime = self$sequentialData$get_VBS()[which(self$sequentialData$get_VBS()$InstanceName == preds[1,]$InstanceName),]$VBS_Runtime
        #parallel_runtimes[which(parallel_runtimes==5000)]<-50000
        #vbs_runtime[which(vbs_runtime==5000)]<-50000
        preds <- cbind(preds,parallel_runtimes)
        preds <- cbind(preds,c(rep(vbs_runtime,nrow(preds))))
        colnames(preds) = c("InstanceName","Solver","SequentialRuntime","PredictedRuntime",
                            "Prediction_StandardError","ParallelRuntime","VBSRuntime")
        
        coreCol = rep(c(1:cores),nrow(preds)/cores)
        if(length(coreCol)<nrow(preds)){
          for(c in 1:(nrow(preds)-length(coreCol))){
            coreCol = append(coreCol, c)
          }
        }
        preds$cores = coreCol
        
        solvedTime = data.frame(matrix(nrow = 0, ncol = 2))
        elem = 1
        for(c in 1:cores){
          solvers = preds[which(preds$cores == c),]$Solver
          total = 0
          for(s in 1:length(solvers)){
            solver_time = preds[which(preds$Solver == solvers[s]),] 
            if(orderBy == "pred"){
              scheduled_for = solver_time$PredictedRuntime
            } else if(orderBy == "pred+SE") {
              scheduled_for = solver_time$PredictedRuntime + solver_time$Prediction_StandardError
            } else if(orderBy == "pred+aSE") {
              scheduled_for = solver_time$PredictedRuntime + a*solver_time$Prediction_StandardError
            } else {
              scheduled_for = self$sequentialData$Cutoff
            }
            if(scheduled_for > self$sequentialData$Cutoff){
              scheduled_for = self$sequentialData$Cutoff
            }
            solver_time = solver_time$ParallelRuntime
            solvedFlag = FALSE
            if(solver_time <= scheduled_for && solver_time <= (self$sequentialData$Cutoff - total)){
              solvedFlag = TRUE
              total = total + solver_time                  
              solvedTime = rbind(solvedTime,c(solvers[s],total))
              elem = elem +1
              if(s < length(solvers)){
                for(r in (s+1):length(solvers)){
                  solvedTime= rbind(solvedTime,c(solvers[r],Inf))
                  elem = elem +1
                }
              }
              break
            } else {
              total = total + scheduled_for
              solvedTime= rbind(solvedTime,c(solvers[s],Inf))
            }
            s = s+1
          }
        }
        colnames(solvedTime) <- c("Solver","ResultingTime")
        colnam = colnames(preds)
        preds = merge(solvedTime,preds,x.by=Solver)[c(colnam, "ResultingTime")]
        if(orderBy == "pred"){
          preds = preds[order(preds$PredictedRuntime),]
        } else if(orderBy == "pred+SE") {
          preds = preds[order(preds$PredictedRuntime + preds$Prediction_StandardError),]
        } else if(orderBy == "pred+aSE"){
          preds = preds[order(preds$PredictedRuntime + a* preds$Prediction_StandardError),]
        }
        
        write.csv(preds,paste0(selectionPath,"/",instance,".csv"),row.names = FALSE)
      }
    },
    
    time_splitting_scheduling_scraper = function(selectionPath,ignoreTimeouts=FALSE, median=FALSE, orderBy = "pred", a = 1){
      tablePar10 <- data.frame(matrix(nrow = 0, ncol = 8+self$sequentialData$n_solvers))
      tableMCP <- data.frame(matrix(nrow = 0, ncol = 8+self$sequentialData$n_solvers))
      tableSuccess <- data.frame(matrix(nrow = 0, ncol = 8+self$sequentialData$n_solvers))
      tableRuntime <- data.frame(matrix(nrow = 0, ncol = 8+self$sequentialData$n_solvers))
      instance_files = list.files(selectionPath,pattern =".csv")
      if(ignoreTimeouts){
        unsolved = lapply(self$sequentialData$unsolvedInstances, function(x) str_split(x,"sat/")[[1]][2])
        unsolved = unlist(unsolved)
        unsolved = paste0(unsolved,".csv")
        instance_files = instance_files[!instance_files %in% unsolved]
      }
      #min parallel runtime of selected schedule
      parallel_runtimes = vector()
      
      sequential_runtimes = vector()
      vbs_runtime = vector()
      nrows = vector()
      data = data.frame(matrix(nrow=0,ncol=8+self$sequentialData$n_solvers))
      for(instance in instance_files){
        #if(".csv" %in% instance){
        csv = read.csv(paste0(selectionPath,"/",instance))
        if(nrow(csv)<self$sequentialData$n_solvers){
          print(paste0("some solvers are not listed: ", instance))
        }
        #}
        #else{
        #  csv = read.csv(paste0(selectionPath,"/",instance,".csv"))
        #}
        min_scheduled_min = min(csv$ResultingTime)
        min_sequential_runtimes = min(csv$SequentialRuntime)
        min_parallel_min = min(csv$ParallelRuntime)
        vbs_runtime = csv$VBSRuntime[1]
        n_selected_solvers = nrow(csv)
        cores = max(csv$cores)
        if(orderBy == "pred"){
          min_pred_min = min(csv$PredictedRuntime)
        } else if(orderBy == "pred+SE"){ 
          min_pred_min = min(csv$PredictedRuntime + csv$Prediction_StandardError)
        } else if(orderBy == "pred+aSE"){
          min_pred_min = min(csv$PredictedRuntime + a* csv$Prediction_StandardError)
        }
        row = c(instance,vbs_runtime,
                min_sequential_runtimes,min_parallel_min,
                min_pred_min,min_scheduled_min,
                n_selected_solvers,cores, orderBy)
        data = rbind(row,data)
      }
      colnames(data)= c("InstanceName","VBSRuntime","min_sequential_runtimes",
                        "min_parallel_runtime", "min_prediction", "min_resulting_time", 
                        "n_selected_solvers","cores","orderBy")
      data[2:7] = lapply(data[2:7],as.numeric)
      data$min_resulting_time[which(is.infinite(data$min_resulting_time))] <- self$sequentialData$Cutoff
      #vbs_not_selected= data[which(data$vbs_runtime!=data$min_sequential_runtimes),]
      vbs = as.numeric(data$VBSRuntime)
      selec_seq = as.numeric(data$min_sequential_runtimes)
      mcp_seq = selec_seq - vbs
      mcp_seq[which(mcp_seq<0)]<-0
      selec_par = as.numeric(data$min_parallel_runtime)
      selec_schedul = as.numeric(data$min_resulting_time)
      mcp_par = selec_par - vbs
      mcp_par[which(mcp_par<0)]<-0
      
      mcp_schedule = selec_schedul - vbs
      mcp_schedule[which(mcp_schedule<0)]<-0
      orderBy = data$orderBy[1]
      #success
      if(median == TRUE){
        rowRuntime <- c("Runtime",median(vbs),median(selec_seq),median(selec_par),median(selec_schedul),
                        median(as.numeric(data$n_selected_solvers)),data$cores[1],mean(selec_seq == vbs), ignoreTimeouts, median, orderBy)
        #success
        #should be mean since will be true false otherwise
        rowSuccess <- c("Success",mean(vbs<self$sequentialData$Cutoff),mean(selec_seq<self$sequentialData$Cutoff),
                        mean(selec_par<self$sequentialData$Cutoff),mean(selec_schedul<self$sequentialData$Cutoff),
                        median(as.numeric(data$n_selected_solvers)),data$cores[1],mean(selec_seq == vbs), ignoreTimeouts, median, orderBy)
        
        rowMCP <- c("MCP",0,median(mcp_seq),median(mcp_par),median(mcp_schedule),
                    median(data$n_selected_solvers),data$cores[1],mean(selec_seq == vbs), ignoreTimeouts, median, orderBy)
        
        vbs[which(vbs>=self$sequentialData$Cutoff)]<-self$sequentialData$Cutoff*10
        selec_seq[which(selec_seq>=self$sequentialData$Cutoff)]<-self$sequentialData$Cutoff*10
        selec_par[which(selec_par>=self$sequentialData$Cutoff)]<-self$sequentialData$Cutoff*10
        selec_schedul[which(selec_schedul>=self$sequentialData$Cutoff)]<-self$sequentialData$Cutoff*10
        
        rowPar10 <- c("Par10",median(vbs),median(selec_seq),median(selec_par),median(selec_schedul),
                      median(as.numeric(data$n_selected_solvers)),data$cores[1],mean(selec_seq == vbs), ignoreTimeouts, median, orderBy)
      } else{
        rowRuntime <- c("Runtime",mean(vbs),mean(selec_seq),mean(selec_par),mean(selec_schedul),
                        mean(as.numeric(data$n_selected_solvers)),data$cores[1],mean(selec_seq == vbs), ignoreTimeouts, median, orderBy)
        #success
        rowSuccess <- c("Success",mean(vbs<self$sequentialData$Cutoff),mean(selec_seq<self$sequentialData$Cutoff),
                        mean(selec_par<self$sequentialData$Cutoff),mean(selec_schedul<self$sequentialData$Cutoff),
                        mean(as.numeric(data$n_selected_solvers)),data$cores[1],mean(selec_seq == vbs), ignoreTimeouts, median, orderBy)
        
        rowMCP <- c("MCP",0,mean(mcp_seq),mean(mcp_par),mean(mcp_schedule),
                    mean(as.numeric(data$n_selected_solvers)),data$cores[1],mean(selec_seq == vbs), ignoreTimeouts, median, orderBy)
        
        vbs[which(vbs>=self$sequentialData$Cutoff)]<-self$sequentialData$Cutoff*10
        selec_seq[which(selec_seq>=self$sequentialData$Cutoff)]<-self$sequentialData$Cutoff*10
        selec_par[which(selec_par>=self$sequentialData$Cutoff)]<-self$sequentialData$Cutoff*10
        selec_schedul[which(selec_schedul>=self$sequentialData$Cutoff)]<-self$sequentialData$Cutoff*10
        
        rowPar10 <- c("Par10",mean(vbs),mean(selec_seq),mean(selec_par),mean(selec_schedul),
                      mean(as.numeric(data$n_selected_solvers)),data$cores[1],mean(selec_seq == vbs), ignoreTimeouts, median, orderBy)
      }
      
      tableRuntime <- rbind(rowRuntime,tableRuntime)
      tableMCP <- rbind(rowMCP,tableMCP)
      tableSuccess <- rbind(rowSuccess,tableSuccess)
      tablePar10 <- rbind(rowPar10,tablePar10)
      colnames(tableRuntime)<- c("metric","VBS","Sequential_time","Parallel_time","Schedule_time",
                                 "#selected_solvers","cores","vbs_selection", "ignoreTimeouts", "median", "orderBy")
      colnames(tableMCP)<- c("metric","VBS","Sequential_time","Parallel_time","Schedule_time",
                             "#selected_solvers","cores","vbs_selection", "ignoreTimeouts", "median", "orderBy")
      colnames(tableSuccess)<- c("metric","VBS","Sequential_time","Parallel_time","Schedule_time",
                                 "#selected_solvers","cores","vbs_selection", "ignoreTimeouts", "median", "orderBy")
      colnames(tablePar10)<- c("metric","VBS","Sequential_time","Parallel_time","Schedule_time",
                               "#selected_solvers","cores","vbs_selection", "ignoreTimeouts", "median", "orderBy")
      return(list(tableRuntime, tableMCP,tablePar10,tableSuccess))
      
    },
    
    selection_based_on_SE_thetaForAlg = function(predictionPath = self$predictionPath, saveTo = self$selectionPath, 
                                                 top_selection = 0,ignoreTimeoutsOnVBS=FALSE, orderBy = "pred", thetas){
      print(predictionPath)
      ds.files = list.files(predictionPath,pattern = ".csv")
      solvers = self$sequentialData$solvers
      solvers = solvers[order(solvers)]
      instances = unlist(lapply(ds.files,function(x){
        str_split(x,".csv")[[1]][1]
      }))
      instances = as.vector(unlist(instances))
      if(ignoreTimeoutsOnVBS) {
        if(is.null(self$sequentialData$solvedInstances)) self$sequentialData$get_VBS()
        instances = instances[which(paste0("sat/",instances) %in% self$sequentialData$solvedInstances)]
      }
      #ids are  ordered
      
      parallelStartSocket(cpus = self$cpus)
      for(instance in instances){
        instanceName = instance
        csv = read.csv(paste0(predictionPath,"/",instanceName[],".csv"))
        if(nrow(csv)<1){
          print("predictions are empty!")
        }
        csv$theta = thetas
        minrow = csv[which(csv$PredictedRuntime == min(csv$PredictedRuntime)),]
        
        min = minrow$PredictedRuntime
        
        #p-delta*se <= minP + deltaPrime*SE_min
        upperbound = min + (minrow$theta*minrow$Prediction_StandardError)
        otherRows = csv[which((csv$PredictedRuntime - (csv$theta*csv$Prediction_StandardError))<= upperbound),]
        #top 5 
        if(orderBy == 'pred'){
          otherRows = otherRows[order(otherRows$PredictedRuntime),]
          if(top_selection != 0 && nrow(otherRows)>top_selection){
            otherRows = otherRows[1:top_selection,]
          }
        } else if(orderBy == 'pred-se'){ 
          otherRows = otherRows[order(otherRows$PredictedRuntime- (otherRows$theta*otherRows$Prediction_StandardError)),]
          if(top_selection != 0 && nrow(otherRows)>top_selection){
            otherRows = otherRows[1:top_selection,]
            otherRows = na.omit(otherRows)
            if(!minrow$Solver %in% otherRows$Solver){
              otherRows = rbind(minrow, otherRows)
              if(nrow(otherRows)>top_selection){
                otherRows = otherRows[1:(nrow(otherRows)-1),]  
              }
            }
          }
        }
        data = otherRows
        data = data[1:5]
        colnames(data)<-c("InstanceName","Solver","ActualRuntime","PredictedRuntime","Prediction_StandardError")
        number_of_solvers = nrow(data)
        actual_parallel_runtime = data.frame()
        path = self$sequentialData$actual_CSV_path
        path = str_split(path,"/")[[1]]
        path = path[-length(path)]
        path = paste0(path, collapse = '/') 
        #this can be improved by a reg model 
        if(number_of_solvers==1){
          filename = paste0(path,"/teton-",self$benchmarks_name,"-",length(self$sequentialData$solvers),"-solvers-solo-replacement.csv")
          actual_parallel_runtime = read.csv(filename)
        } else if (number_of_solvers<10){
          filename = paste0(path,"/teton-",self$benchmarks_name,"-",length(self$sequentialData$solvers),"-solvers-",number_of_solvers,
                            "-parallel-replacement.csv")
          actual_parallel_runtime = read.csv(filename)
        } else if(number_of_solvers>31){
          filename = paste0(path,"/teton-",self$benchmarks_name,"-",length(self$sequentialData$solvers),"-solvers-32-parallel-replacement.csv")
          actual_parallel_runtime = read.csv(filename)    
        } else if(number_of_solvers>=10 ){
          core = round(number_of_solvers,-1)
          filename = paste0(path,"/teton-",self$benchmarks_name,"-",length(self$sequentialData$solvers),"-solvers-",core,"-parallel-replacement.csv")
          actual_parallel_runtime = read.csv(filename)
        } 
        parallel_runtimes <- vector()
        csvrow = actual_parallel_runtime[which(actual_parallel_runtime$InstanceName == data[1,]$InstanceName),]
        for(m in 0:nrow(data)){
          parallel_runtimes <- append(parallel_runtimes,csvrow[data[m,]$Solver])
          parallel_runtimes = unname(unlist(parallel_runtimes))
        }
        vbs_runtime = self$sequentialData$get_VBS()[which(self$sequentialData$get_VBS()$InstanceName == data[1,]$InstanceName),]$VBS_Runtime
        #parallel_runtimes[which(parallel_runtimes==5000)]<-50000
        #vbs_runtime[which(vbs_runtime==5000)]<-50000
        data <- cbind(data,parallel_runtimes)
        data <- cbind(data,c(rep(vbs_runtime,number_of_solvers)))
        #colnames(data) = c("instance","solver","sequential_par10","prediction","standardError","parallel_par10","vbs_par10")
        colnames(data) = c("InstanceName","Solver","SequentialRuntime","PredictedRuntime",
                           "Prediction_StandardError","ParallelRuntime","VBSRuntime")
        for(solver in solvers){
          data[paste0(solver,"_theta")] <- rep(x=csv[which(csv$Solver == solver),]$theta,times = nrow(data))
        }
        data$orderBy = orderBy
        if(saveTo!="")
          write.csv(data,paste0(saveTo,"/",instanceName,".csv"),row.names = FALSE)
      }
      parallelStop()
    },
    
    #nemidonam ina chian 
    selection_based_on_SE_validation = function(predictionPath, saveTo, method_number = 1, 
                                                top_selection = 0,ignoreTimeoutsOnVBS=TRUE){
      ds.files = list.files(predictionPath,pattern = ".csv")
      solvers = self$sequentialData$solvers
      solvers = solvers[order(solvers)]
      instances = unlist(lapply(ds.files,function(x){
        str_split(x,".csv")[[1]][1]
      }))
      instances = as.vector(unlist(instances))
      if(ignoreTimeoutsOnVBS) {
        if(is.null(self$sequentialData$solvedInstances)) self$sequentialData$get_VBS()
        instances = instances[which(paste0("sat/",instances) %in% self$sequentialData$solvedInstances)] 
      }
      #ids are  ordered
      
      parallelStartSocket(cpus = self$cpus)
      for(instance in instances){
        instanceName = instance
        csv = read.csv(paste0(predictionPath,"/",instanceName[],".csv"))
        for(iter in c(1:max(csv$Iteration))){
          csv_iter = csv[which(csv$Iteration == iter),]
          minrow = csv_iter[which(csv_iter$PredictedRuntime == min(csv_iter$PredictedRuntime)),]
          min = minrow$PredictedRuntime
          #don't do selection
          if(method_number==-1){
            otherRows = csv_iter
            otherRows = otherRows[order(otherRows$PredictedRuntime),]
            data = otherRows
          }
          
          #top selections
          else if(method_number==0){
            otherRows = csv_iter
            otherRows = otherRows[order(otherRows$PredictedRuntime),]
            if(top_selection != 0 && nrow(otherRows)>top_selection){
              otherRows = otherRows[1:top_selection,]
              otherRows = na.omit(otherRows)
              if(!minrow$Solver %in% otherRows$Solver){
                otherRows = rbind(minrow, otherRows)
                if(nrow(otherRows)>top_selection){
                  otherRows = otherRows[1:(nrow(otherRows)-1),]  
                }
                
              }
            }
            data = otherRows
          }
          
          #pred >= [minpred + SE], then order by pred value
          else if(method_number==1){
            upperbound = min + minrow$Prediction_StandardError
            lowerbound = min
            otherRows = csv_iter[which(csv_iter$PredictedRuntime<= upperbound),]
            otherRows = otherRows[which(otherRows$PredictedRuntime>=lowerbound),]
            #top 5 
            otherRows = otherRows[order(otherRows$PredictedRuntime),]
            if(top_selection != 0 && nrow(otherRows)>top_selection){
              otherRows = otherRows[1:top_selection,]
              otherRows = na.omit(otherRows)
              if(!minrow$Solver %in% otherRows$Solver){
                otherRows = rbind(minrow, otherRows)
                if(nrow(otherRows)>top_selection){
                  otherRows = otherRows[1:(nrow(otherRows)-1),]  
                }
                
              }
            }
            data = otherRows
          }
          
          #pred >= [minpred + SE], then order by (pred-SE)
          else if(method_number==2){
            upperbound = min + minrow$Prediction_StandardError
            lowerbound = min
            otherRows = csv_iter[which(csv_iter$PredictedRuntime<= upperbound),]
            otherRows = otherRows[which(otherRows$PredictedRuntime>=lowerbound),]
            #top 5 
            otherRows = otherRows[order(otherRows$PredictedRuntime- otherRows$Prediction_StandardError),]
            if(top_selection != 0 && nrow(otherRows)>top_selection){
              otherRows = otherRows[1:top_selection,]
              otherRows = na.omit(otherRows)
              if(!minrow$Solver %in% otherRows$Solver){
                otherRows = rbind(minrow, otherRows)
                if(nrow(otherRows)>top_selection){
                  otherRows = otherRows[1:(nrow(otherRows)-1),]  
                }
                
              }
            }
            data = otherRows
          }
          
          #lowebound as good as min pred, [pred-SE]<=minpred, then order by pred value
          else if(method_number==3){
            upperbound = min
            otherRows = csv_iter[which((csv_iter$PredictedRuntime-csv_iter$Prediction_StandardError)<= upperbound),]
            otherRows = otherRows[order(otherRows$PredictedRuntime),]
            #top 5 
            if(top_selection!=0){
              if(nrow(otherRows)>top_selection){
                otherRows = otherRows[1:top_selection,]
                otherRows = na.omit(otherRows)
                if(!minrow$Solver %in% otherRows$Solver){
                  otherRows = rbind(minrow, otherRows)
                  if(nrow(otherRows)>top_selection){
                    otherRows = otherRows[1:(nrow(otherRows)-1),]  
                  }
                  
                }
              }
            }
            data = otherRows
          }
          
          #lowebound as good as min pred, [pred-SE]<=minpred, then order by (pred-SE)
          else if(method_number==4){
            upperbound = min
            otherRows = csv_iter[which((csv_iter$PredictedRuntime-csv_iter$Prediction_StandardError)<= upperbound),]
            otherRows = otherRows[order(otherRows$PredictedRuntime - otherRows$Prediction_StandardError),]
            #top 5 
            if(top_selection!=0){
              if(nrow(otherRows)>top_selection){
                otherRows = otherRows[1:top_selection,]
                otherRows = na.omit(otherRows)
                if(!minrow$Solver %in% otherRows$Solver){
                  otherRows = rbind(minrow, otherRows)
                  if(nrow(otherRows)>top_selection){
                    otherRows = otherRows[1:(nrow(otherRows)-1),]  
                  }
                  
                }
              }
            }
            data = otherRows
          }
          
          #lower as good as min, but nor lower than [minpred-SE], [minpred-SE]<=[pred-SE]<=minpred, then order by pred value
          else if(method_number==5){
            upperbound = min
            lowerbound = min - minrow$Prediction_StandardError
            otherRows = csv_iter[which((csv_iter$PredictedRuntime-csv_iter$Prediction_StandardError)<= upperbound),]
            otherRows = otherRows[which((otherRows$PredictedRuntime-otherRows$Prediction_StandardError)>=lowerbound),]
            otherRows = otherRows[order(otherRows$PredictedRuntime),]
            if(top_selection!=0){
              if(nrow(otherRows)>top_selection){
                otherRows = otherRows[1:top_selection,]
                otherRows = na.omit(otherRows)
                if(!minrow$Solver %in% otherRows$Solver){
                  otherRows = rbind(minrow, otherRows)
                  if(nrow(otherRows)>top_selection){
                    otherRows = otherRows[1:(nrow(otherRows)-1),]  
                  }
                  
                }
              }
            }
            data = otherRows
          }
          
          #lower as good as min, but nor lower than [minpred-SE], [minpred-SE]<=[pred-SE]<=minpred, then order by (pred-SE)
          else if(method_number==6){
            upperbound = min
            lowerbound = min - minrow$Prediction_StandardError
            otherRows = csv_iter[which((csv_iter$PredictedRuntime-csv_iter$Prediction_StandardError)<= upperbound),]
            otherRows = otherRows[which((otherRows$PredictedRuntime-otherRows$Prediction_StandardError)>=lowerbound),]
            otherRows = otherRows[order((otherRows$PredictedRuntime-otherRows$Prediction_StandardError)),]
            if(top_selection!=0){
              if(nrow(otherRows)>top_selection){
                otherRows = otherRows[1:top_selection,]
                otherRows = na.omit(otherRows)
                if(!minrow$Solver %in% otherRows$Solver){
                  otherRows = rbind(minrow, otherRows)
                  if(nrow(otherRows)>top_selection){
                    otherRows = otherRows[1:(nrow(otherRows)-1),]  
                  }
                  
                }
              }
            }
            data = otherRows
          }
          
          #the following ones are statistical selection - they may be wrong
          colnames(data)<-c("InstanceName","Solver","SequentialRuntime","PredictedRuntime","Prediction_StandardError","Iteration","Validation_set","solver_prediction_rank")
          number_of_solvers = nrow(data)
          actual_parallel_runtime = data.frame()
          path = self$sequentialData$actual_CSV_path
          path = str_split(path,"/")[[1]]
          path = path[-length(path)]
          path = paste0(path, collapse = '/') 
          #this can be improved by a reg model 
          if(number_of_solvers==1){
            filename = paste0(path,"/teton-SAT2018-39-solvers-solo-replacement.csv")
            actual_parallel_runtime = read.csv(filename)
          } else if (number_of_solvers<10){
            filename = paste0(path,"/teton-SAT2018-39-solvers-",number_of_solvers,
                              "-parallel-replacement.csv")
            actual_parallel_runtime = read.csv(filename)
          } else if(number_of_solvers>31){
            filename = paste0(path,"/teton-SAT2018-39-solvers-32-parallel-replacement.csv")
            actual_parallel_runtime = read.csv(filename)    
          } else if(number_of_solvers>=10 ){
            core = round(number_of_solvers,-1)
            filename = paste0(path,"/teton-SAT2018-39-solvers-",core,"-parallel-replacement.csv")
            actual_parallel_runtime = read.csv(filename)
          } 
          parallel_runtimes <- vector()
          csvrow = actual_parallel_runtime[which(actual_parallel_runtime$InstanceName == data[1,]$InstanceName),]
          for(m in 0:nrow(data)){
            parallel_runtimes <- append(parallel_runtimes,csvrow[data[m,]$Solver])
            parallel_runtimes = unname(unlist(parallel_runtimes))
          }
          vbs_runtime = self$sequentialData$get_VBS()[which(self$sequentialData$get_VBS()$InstanceName == data[1,]$InstanceName),]$VBS_Runtime
          #parallel_runtimes[which(parallel_runtimes==5000)]<-50000
          #vbs_runtime[which(vbs_runtime==5000)]<-50000
          data <- cbind(data,parallel_runtimes)
          data <- cbind(data,c(rep(vbs_runtime,number_of_solvers)))
          #colnames(data) = c("instance","solver","sequential_par10","prediction","standardError","parallel_par10","vbs_par10")
          colnames(data) = c("InstanceName","Solver","SequentialRuntime","PredictedRuntime",
                             "Prediction_StandardError","Iteration","Validation_set","solver_prediction_rank","ParallelRuntime","VBSRuntime")
          write.csv(data,paste0(saveTo,"/",instanceName,"_",iter,"_fold.csv"),row.names = FALSE)
        }
      }
      parallelStop()
    },
    
    selection_based_on_SE_iterations = function(selectionpath, saveTo, method_number = 2, 
                                                top_selection = 0,ignoreTimeoutsOnVBS=TRUE){
      ds.files = list.files(predictionPath,pattern = ".csv")
      solvers = self$sequentialData$solvers
      solvers = solvers[order(solvers)]
      instances = unlist(lapply(ds.files,function(x){
        str_split(x,".csv")[[1]][1]
      }))
      instances = as.vector(unlist(instances))
      if(ignoreTimeoutsOnVBS) {
        if(is.null(self$sequentialData$solvedInstances)) self$sequentialData$get_VBS()
        instances = instances[which(paste0("sat/",instances) %in% self$sequentialData$solvedInstances)]
      }
      #ids are  ordered
      
      parallelStartSocket(cpus = self$cpus)
      for(instance in instances){
        instanceName = instance
        csv = read.csv(paste0(predictionPath,"/",instanceName[],".csv"))
        minrow = csv[which(csv$PredictedRuntime == min(csv$PredictedRuntime)),]
        min = minrow$PredictedRuntime
        #don't do selection
        if(method_number==-1){
          otherRows = csv
          otherRows = otherRows[order(otherRows$PredictedRuntime),]
          data = otherRows
        }
        #lowebound as good as min pred
        else if(method_number==2){
          upperbound = min
          otherRows = csv[which((csv$PredictedRuntime-csv$Prediction_StandardError)<= upperbound),]
          otherRows = otherRows[order(otherRows$PredictedRuntime - otherRows$Prediction_StandardError),]
          #top 5 
          if(top_selection!=0){
            if(nrow(otherRows)>top_selection){
              otherRows = otherRows[1:top_selection,]
            }
          }
          data = otherRows
        }
        colnames(data)<-c("InstanceName","Solver","ActualRuntime","PredictedRuntime","Prediction_StandardError")
        number_of_solvers = nrow(data)
        actual_parallel_runtime = data.frame()
        path = self$sequentialData$actual_CSV_path
        path = str_split(path,"/")[[1]]
        path = path[-length(path)]
        path = paste0(path, collapse = '/') 
        #this can be improved by a reg model 
        if(number_of_solvers==1){
          filename = paste0(path,"/teton-SAT2018-39-solvers-solo-replacement.csv")
          actual_parallel_runtime = read.csv(filename)
        } else if (number_of_solvers<10){
          filename = paste0(path,"/teton-SAT2018-39-solvers-",number_of_solvers,
                            "-parallel-replacement.csv")
          actual_parallel_runtime = read.csv(filename)
        } else if(number_of_solvers>31){
          filename = paste0(path,"/teton-SAT2018-39-solvers-32-parallel-replacement.csv")
          actual_parallel_runtime = read.csv(filename)    
        } else if(number_of_solvers>=10 ){
          core = round(number_of_solvers,-1)
          filename = paste0(path,"/teton-SAT2018-39-solvers-",core,"-parallel-replacement.csv")
          actual_parallel_runtime = read.csv(filename)
        } 
        parallel_runtimes <- vector()
        csvrow = actual_parallel_runtime[which(actual_parallel_runtime$InstanceName == data[1,]$InstanceName),]
        for(m in 0:nrow(data)){
          parallel_runtimes <- append(parallel_runtimes,csvrow[data[m,]$Solver])
          parallel_runtimes = unname(unlist(parallel_runtimes))
        }
        vbs_runtime = self$sequentialData$get_VBS()[which(self$sequentialData$get_VBS()$InstanceName == data[1,]$InstanceName),]$VBS_Runtime
        #parallel_runtimes[which(parallel_runtimes==5000)]<-50000
        #vbs_runtime[which(vbs_runtime==5000)]<-50000
        data <- cbind(data,parallel_runtimes)
        data <- cbind(data,c(rep(vbs_runtime,number_of_solvers)))
        #colnames(data) = c("instance","solver","sequential_par10","prediction","standardError","parallel_par10","vbs_par10")
        colnames(data) = c("InstanceName","Solver","SequentialRuntime","PredictedRuntime",
                           "Prediction_StandardError","ParallelRuntime","VBSRuntime")
        write.csv(data,paste0(saveTo,"/",instanceName,".csv"),row.names = FALSE)
      }
      parallelStop()
    },
    
    plot_selection_increasing_core_folds = function(selection_valid_path){
      #library(plotly)
      all_instances = list.files(selection_valid_path,".csv")
      for(ins in 1:length(all_instances)){
        sample_instance = all_instances[ins:(ins+10)]
        ins = ins+10
        csv = read.csv(paste0(selection_valid_path,sample_instance[1]))
        fig = plot_ly()
        fig = fig%>%add_trace(x=c(1:nrow(csv)),y= csv$VBSRuntime,mode='lines',name="VBS",line = list(width = 4, dash = "dot"))
        for(j in 1:length(sample_instance)){
          csv = read.csv(paste0(selection_valid_path,sample_instance[j]))
          csv$increasing_cores_runtime = c(rep(0,nrow(csv)))
          for(i in 1:nrow(csv)){
            core =0
            if(i ==1){
              core = "solo"
            } else if(i<=10){
              core = i
            } else if(i<=14){
              core = 10
            } else if(i <= 24){
              core = 20
            } else if(i<=31){
              core = 30
            } else if(i>=32){
              core =32
            }
            if(core == "solo"){
              runtimes = read.csv(paste0("~/Documents/Portfolio-Scheduling/originalCSVs-Teton_SAT18/without_twoinstances/teton-SAT2018-39-solvers-",core,"-replacement.csv"))
            } else{
              runtimes = read.csv(paste0("~/Documents/Portfolio-Scheduling/originalCSVs-Teton_SAT18/without_twoinstances/teton-SAT2018-39-solvers-",core,"-parallel-replacement.csv"))
            }
            instanceName = str_split(sample_instance,".csv")[[1]][1]
            instanceName = paste0(head(str_split(instanceName,"_")[[1]],-2),collapse="_")
            
            related_row = runtimes[which(runtimes$InstanceName==paste0("sat/",instanceName)),]
            solvers = csv[1:i,]$Solver
            csv$increasing_cores_runtime[i] = min(unlist(unname(related_row[solvers])))
          }
          fig = fig%>%add_trace(x=c(1:nrow(csv)),y= csv$increasing_cores_runtime,mode='lines+markers',name=paste0("Fold ",j))
        }
        htmlwidgets::saveWidget(fig,paste0("~/Documents/ranger_results/preds_valid/validation/plot/",instanceName,".html"))
      }
      
    },
    
    plot_selection_increasing_core_expected = function(selection_valid_exptected_path){
      #library(plotly)
      all_instances = list.files(selection_valid_exptected_path,".csv")
      for(ins in 1:length(all_instances)){
        sample_instance = all_instances[ins]
        csv = read.csv(paste0(selection_valid_exptected_path,sample_instance))
        fig = plot_ly()
        fig = fig%>%add_trace(x=c(1:nrow(csv)),y= csv$VBSRuntime,mode='lines',name="VBS",line = list(width = 4, dash = "dot"))
        csv$increasing_cores_runtime = c(rep(0,nrow(csv)))
        for(i in 1:nrow(csv)){
          core =0
          if(i ==1){
            core = "solo"
          } else if(i<=10){
            core = i
          } else if(i<=14){
            core = 10
          } else if(i <= 24){
            core = 20
          } else if(i<=31){
            core = 30
          } else if(i>=32){
            core =32
          }
          if(core == "solo"){
            runtimes = read.csv(paste0("~/Documents/Portfolio-Scheduling/originalCSVs-Teton_SAT18/without_twoinstances/teton-SAT2018-39-solvers-",core,"-replacement.csv"))
          } else{
            runtimes = read.csv(paste0("~/Documents/Portfolio-Scheduling/originalCSVs-Teton_SAT18/without_twoinstances/teton-SAT2018-39-solvers-",core,"-parallel-replacement.csv"))
          }
          instanceName = str_split(sample_instance,".csv")[[1]][1]
          related_row = runtimes[which(runtimes$InstanceName==paste0("sat/",instanceName)),]
          solvers = csv[1:i,]$Solver
          csv$increasing_cores_runtime[i] = min(unlist(unname(related_row[solvers])))
          csv$increasing_cores_prediction[i] = min(csv$ExpectedPrediction[1:i])  
          csv$increasing_cores_prediction_lowerbound[i] = min(csv$ExpectedLowebound[1:i])
          
        }
        fig = fig%>%add_trace(x=c(1:nrow(csv)),y= csv$increasing_cores_runtime,mode='lines+markers',name="Actual Parallel")
        fig = fig%>%add_trace(x=c(1:nrow(csv)),y= csv$increasing_cores_prediction,mode='lines+markers',name="Expected Prediction")
        fig = fig%>%add_trace(x=c(1:nrow(csv)),y= csv$increasing_cores_prediction_lowerbound,mode='lines+markers',name="Expected Lowerbound")
        
        htmlwidgets::saveWidget(fig,paste0("~/Documents/ranger_results/preds_valid/validation/plot/",instanceName,".html"))
      }
      
    },
    
    #get mcp, par10, runtime and success rate of selection
    get_summary_selection_result = function(selectionPath,ignoreTimeouts=TRUE, method_number = 0, median = FALSE){
      tablePar10 <- data.frame(matrix(nrow = 0, ncol = 13))
      tableMCP <- data.frame(matrix(nrow = 0, ncol = 13))
      tableSuccess <- data.frame(matrix(nrow = 0, ncol = 13))
      tableRuntime <- data.frame(matrix(nrow = 0, ncol = 13))
      instance_files = list.files(selectionPath,pattern =".csv")
      if(ignoreTimeouts){
        unsolved = lapply(self$sequentialData$unsolvedInstances, function(x) str_split(x,"sat/")[[1]][2])
        unsolved = unlist(unsolved)
        unsolved = paste0(unsolved,".csv")
        instance_files = instance_files[!instance_files %in% unsolved]
      }
      #min parallel runtime of selected schedule
      parallel_runtimes = vector()
      
      sequential_runtimes = vector()
      vbs_runtime = vector()
      nrows = vector()
      data = data.frame(matrix(nrow=0,ncol=11))
      for(instance in instance_files){
        #if(".csv" %in% instance){
        csv = read.csv(paste0(selectionPath,"/",instance))
        #}
        #else{
        #  csv = read.csv(paste0(selectionPath,"/",instance,".csv"))
        #}
        min_parallel_min = min(csv$ParallelRuntime)
        min_sequential_runtimes = min(csv$SequentialRuntime)
        vbs_runtime = csv$VBSRuntime[1]
        n_selected_solvers = nrow(csv)
        row = c(instance,vbs_runtime,min_sequential_runtimes,min_parallel_min,n_selected_solvers, csv$delta[1], csv$delta_prime[1], csv$alpha[1], csv$orderBy[1])
        data = rbind(row,data)
      }
      colnames(data)= c("InstanceName","VBSRuntime","min_sequential_runtimes",
                        "min_parallel_runtime","n_selected_solvers", "delta", "delta_prime", "alpha", "orderBy")
      #vbs_not_selected= data[which(data$vbs_runtime!=data$min_sequential_runtimes),]
      vbs = as.numeric(data$VBSRuntime)
      selec_seq = as.numeric(data$min_sequential_runtimes)
      mcp_seq = selec_seq - vbs
      mcp_seq[which(mcp_seq<0)]<-0
      selec_par = as.numeric(data$min_parallel_runtime)
      mcp_par = selec_par - vbs
      mcp_par[which(mcp_par<0)]<-0
      delta = data$delta[1]
      delta_prime = data$delta_prime[1]
      alpha = data$alpha[1]
      orderBy = data$orderBy[1]
      #success
      if(median == TRUE){
        rowRuntime <- c("Runtime",method_number,median(vbs),median(selec_seq),median(selec_par),
                        median(as.numeric(data$n_selected_solvers)),mean(selec_seq == vbs), delta, delta_prime, alpha, ignoreTimeouts, median, orderBy)
        #success
        #should be mean since will be true false otherwise
        rowSuccess <- c("Success",method_number,mean(vbs<self$sequentialData$Cutoff),mean(selec_seq<self$sequentialData$Cutoff),mean(selec_par<self$sequentialData$Cutoff),
                        median(as.numeric(data$n_selected_solvers)),mean(selec_seq == vbs), delta, delta_prime, alpha, ignoreTimeouts, median, orderBy)
        
        rowMCP <- c("MCP",method_number,0,median(mcp_seq),median(mcp_par),
                    median(as.numeric(data$n_selected_solvers)),mean(selec_seq == vbs), delta, delta_prime, alpha, ignoreTimeouts, median, orderBy)
        
        vbs[which(vbs>=self$sequentialData$Cutoff)]<-self$sequentialData$Cutoff*10
        selec_seq[which(selec_seq>=self$sequentialData$Cutoff)]<-self$sequentialData$Cutoff*10
        selec_par[which(selec_par>=self$sequentialData$Cutoff)]<-self$sequentialData$Cutoff*10
        
        rowPar10 <- c("Par10",method_number,median(vbs),median(selec_seq),median(selec_par),
                      median(as.numeric(data$n_selected_solvers)),mean(selec_seq == vbs), delta, delta_prime, alpha, ignoreTimeouts, median, orderBy)
      } else{
        rowRuntime <- c("Runtime",method_number,mean(vbs),mean(selec_seq),mean(selec_par),
                        mean(as.numeric(data$n_selected_solvers)),mean(selec_seq == vbs), delta, delta_prime, alpha, ignoreTimeouts, median, orderBy)
        #success
        rowSuccess <- c("Success",method_number,mean(vbs<self$sequentialData$Cutoff*10),mean(selec_seq<self$sequentialData$Cutoff*10),mean(selec_par<self$sequentialData$Cutoff*10),
                        mean(as.numeric(data$n_selected_solvers)),mean(selec_seq == vbs), delta, delta_prime, alpha, ignoreTimeouts, median, orderBy)
        
        rowMCP <- c("MCP",method_number,0,mean(mcp_seq),mean(mcp_par),
                    mean(as.numeric(data$n_selected_solvers)),mean(selec_seq == vbs), delta, delta_prime, alpha, ignoreTimeouts, median, orderBy)
        
        vbs[which(vbs>=self$sequentialData$Cutoff)]<-self$sequentialData$Cutoff*10
        selec_seq[which(selec_seq>=self$sequentialData$Cutoff)]<-self$sequentialData$Cutoff*10
        selec_par[which(selec_par>=self$sequentialData$Cutoff)]<-self$sequentialData$Cutoff*10
        
        rowPar10 <- c("Par10",method_number,mean(vbs),mean(selec_seq),mean(selec_par),
                      mean(as.numeric(data$n_selected_solvers)),mean(selec_seq == vbs), delta, delta_prime, alpha, ignoreTimeouts, median, orderBy)
      }
      
      tableRuntime <- rbind(rowRuntime,tableRuntime)
      tableMCP <- rbind(rowMCP,tableMCP)
      tableSuccess <- rbind(rowSuccess,tableSuccess)
      tablePar10 <- rbind(rowPar10,tablePar10)
      colnames(tableRuntime)<- c("metric","method","VBS","Sequential_time","Parallel_time",
                                 "#selected_solvers","vbs_selection", "delta", "delta_prime", "alpha", "ignoreTimeouts", "median", "orderBy")
      colnames(tableMCP)<- c("metric","method","VBS","Sequential_time","Parallel_time",
                             "#selected_solvers","vbs_selection", "delta", "delta_prime", "alpha", "ignoreTimeouts", "median", "orderBy")
      colnames(tableSuccess)<- c("metric","method","VBS","Sequential_time","Parallel_time",
                                 "#selected_solvers","vbs_selection", "delta", "delta_prime", "alpha", "ignoreTimeouts", "median", "orderBy")
      colnames(tablePar10)<- c("metric","method","VBS","Sequential_time","Parallel_time",
                               "#selected_solvers","vbs_selection", "delta", "delta_prime", "alpha", "ignoreTimeouts", "median", "orderBy")
      return(list(tableRuntime, tableMCP,tablePar10,tableSuccess))
    },
    
    #get mcp, par10, runtime and success rate of selection
    get_selection_result_all_instances = function(selectionPath,ignoreTimeouts=TRUE, method_number = 0, median = FALSE){
      tablePar10 <- data.frame(matrix(nrow = 0, ncol = 13))
      tableMCP <- data.frame(matrix(nrow = 0, ncol = 13))
      tableSuccess <- data.frame(matrix(nrow = 0, ncol = 13))
      tableRuntime <- data.frame(matrix(nrow = 0, ncol = 13))
      instance_files = list.files(selectionPath,pattern =".csv")
      if(ignoreTimeouts){
        unsolved = lapply(self$sequentialData$unsolvedInstances, function(x) str_split(x,"sat/")[[1]][2])
        unsolved = unlist(unsolved)
        unsolved = paste0(unsolved,".csv")
        instance_files = instance_files[!instance_files %in% unsolved]
      }
      #min parallel runtime of selected schedule
      parallel_runtimes = vector()
      
      sequential_runtimes = vector()
      vbs_runtime = vector()
      nrows = vector()
      data = data.frame(matrix(nrow=0,ncol=11))
      for(instance in instance_files){
        #if(".csv" %in% instance){
        csv = read.csv(paste0(selectionPath,"/",instance))
        #}
        #else{
        #  csv = read.csv(paste0(selectionPath,"/",instance,".csv"))
        #}
        min_parallel_min = min(csv$ParallelRuntime)
        min_sequential_runtimes = min(csv$SequentialRuntime)
        vbs_runtime = csv$VBSRuntime[1]
        n_selected_solvers = nrow(csv)
        row = c(instance,vbs_runtime,min_sequential_runtimes,min_parallel_min,n_selected_solvers, csv$delta[1], csv$delta_prime[1], csv$alpha[1], csv$orderBy[1])
        data = rbind(row,data)
      }
      colnames(data)= c("InstanceName","VBSRuntime","min_sequential_runtimes",
                        "min_parallel_runtime","n_selected_solvers", "delta", "delta_prime", "alpha", "orderBy")
      return(data)
    },
    
    #get mcp, par10, runtime and success rate of selection
    get_summary_selection_result_differentThetasEachAlg = function(selectionPath,ignoreTimeouts=TRUE, method_number = 0, median = FALSE){
      tablePar10 <- data.frame(matrix(nrow = 0, ncol = 8+self$sequentialData$n_solvers))
      tableMCP <- data.frame(matrix(nrow = 0, ncol = 8+self$sequentialData$n_solvers))
      tableSuccess <- data.frame(matrix(nrow = 0, ncol = 8+self$sequentialData$n_solvers))
      tableRuntime <- data.frame(matrix(nrow = 0, ncol = 8+self$sequentialData$n_solvers))
      instance_files = list.files(selectionPath,pattern =".csv")
      if(ignoreTimeouts){
        unsolved = lapply(self$sequentialData$unsolvedInstances, function(x) str_split(x,"sat/")[[1]][2])
        unsolved = unlist(unsolved)
        unsolved = paste0(unsolved,".csv")
        instance_files = instance_files[!instance_files %in% unsolved]
      }
      #min parallel runtime of selected schedule
      parallel_runtimes = vector()
      
      sequential_runtimes = vector()
      vbs_runtime = vector()
      nrows = vector()
      data = data.frame(matrix(nrow=0,ncol=8+self$sequentialData$n_solvers))
      for(instance in instance_files){
        if(".csv" %in% instance){
          csv = read.csv(paste0(selectionPath,"/",instance))
        }
        else{
          csv = read.csv(paste0(selectionPath,"/",instance,".csv"))
        }
        min_parallel_min = min(csv$ParallelRuntime)
        min_sequential_runtimes = min(csv$SequentialRuntime)
        vbs_runtime = csv$VBSRuntime[1]
        n_selected_solvers = nrow(csv)
        row = c(instance,vbs_runtime,min_sequential_runtimes,min_parallel_min,n_selected_solvers, unlist(unname(csv[1,8:length(colnames(csv))])))
        data = rbind(row,data)
      }
      colnames(data)= c("InstanceName","VBSRuntime","min_sequential_runtimes",
                        "min_parallel_runtime","n_selected_solvers", colnames(csv)[9:length(colnames(csv))-1],"orderBy")
      vbs_not_selected= data[which(data$vbs_runtime!=data$min_sequential_runtimes),]
      vbs = as.numeric(data$VBSRuntime)
      selec_seq = as.numeric(data$min_sequential_runtimes)
      mcp_seq = selec_seq - vbs
      mcp_seq[which(mcp_seq<0)]<-0
      selec_par = as.numeric(data$min_parallel_runtime)
      mcp_par = selec_par - vbs
      mcp_par[which(mcp_par<0)]<-0
      theta = unlist(unname(data[1,7:length(colnames(data))-1]))
      orderBy = data$orderBy[1]
      success
      if(median == TRUE){
        rowRuntime <- c("Runtime",method_number,median(vbs),median(selec_seq),median(selec_par),
                        median(as.numeric(data$n_selected_solvers)),mean(selec_seq == vbs), theta, ignoreTimeouts, median, orderBy)
        #succes should be mean since will be true false otherwise
        rowSuccess <- c("Success",method_number,mean(vbs<self$sequentialData$Cutoff),mean(selec_seq<self$sequentialData$Cutoff),mean(selec_par<self$sequentialData$Cutoff),
                        median(as.numeric(data$n_selected_solvers)),mean(selec_seq == vbs), theta, ignoreTimeouts, median, orderBy)
        
        rowMCP <- c("MCP",method_number,0,median(mcp_seq),median(mcp_par),
                    median(as.numeric(data$n_selected_solvers)),mean(selec_seq == vbs), theta, ignoreTimeouts, median, orderBy)
        
        vbs[which(vbs>=self$sequentialData$Cutoff)]<-self$sequentialData$Cutoff*10
        selec_seq[which(selec_seq>=self$sequentialData$Cutoff)]<-self$sequentialData$Cutoff*10
        selec_par[which(selec_par>=self$sequentialData$Cutoff)]<-self$sequentialData$Cutoff*10
        
        rowPar10 <- c("Par10",method_number,median(vbs),median(selec_seq),median(selec_par),
                      median(as.numeric(data$n_selected_solvers)),mean(selec_seq == vbs), theta, ignoreTimeouts, median, orderBy)
      } else{
        rowRuntime <- c("Runtime",method_number,mean(vbs),mean(selec_seq),mean(selec_par),
                        mean(as.numeric(data$n_selected_solvers)),mean(selec_seq == vbs), theta , ignoreTimeouts, median, orderBy)
        success
        rowSuccess <- c("Success",method_number,mean(vbs<self$sequentialData$Cutoff),mean(selec_seq<self$sequentialData$Cutoff),mean(selec_par<self$sequentialData$Cutoff),
                        mean(as.numeric(data$n_selected_solvers)),mean(selec_seq == vbs), theta, ignoreTimeouts, median, orderBy)
        
        rowMCP <- c("MCP",method_number,0,mean(mcp_seq),mean(mcp_par),
                    mean(as.numeric(data$n_selected_solvers)),mean(selec_seq == vbs), theta, ignoreTimeouts, median, orderBy)
        
        vbs[which(vbs>=self$sequentialData$Cutoff)]<-self$sequentialData$Cutoff*10
        selec_seq[which(selec_seq>=self$sequentialData$Cutoff)]<-self$sequentialData$Cutoff*10
        selec_par[which(selec_par>=self$sequentialData$Cutoff)]<-self$sequentialData$Cutoff*10
        
        rowPar10 <- c("Par10",method_number,mean(vbs),mean(selec_seq),mean(selec_par),
                      mean(as.numeric(data$n_selected_solvers)),mean(selec_seq == vbs), theta, ignoreTimeouts, median, orderBy)
      }
      
      tableRuntime <- rbind(rowRuntime,tableRuntime)
      tableMCP <- rbind(rowMCP,tableMCP)
      tableSuccess <- rbind(rowSuccess,tableSuccess)
      tablePar10 <- rbind(rowPar10,tablePar10)
      colnames(tableRuntime)<- c("metric","method","VBS","Sequential_time","Parallel_time",
                                 "#selected_solvers","vbs_selection", colnames(data)[7:length(data)-1], "ignoreTimeouts", "median", "orderBy")
      colnames(tableMCP)<- c("metric","method","VBS","Sequential_time","Parallel_time",
                             "#selected_solvers","vbs_selection", colnames(data)[7:length(data)-1], "ignoreTimeouts", "median", "orderBy")
      colnames(tableSuccess)<- c("metric","method","VBS","Sequential_time","Parallel_time",
                                 "#selected_solvers","vbs_selection", colnames(data)[7:length(data)-1], "ignoreTimeouts", "median", "orderBy")
      colnames(tablePar10)<- c("metric","method","VBS","Sequential_time","Parallel_time",
                               "#selected_solvers","vbs_selection", colnames(data)[7:length(data)-1], "ignoreTimeouts", "median", "orderBy")
      return(list(tableRuntime, tableMCP,tablePar10,tableSuccess))
    },
    
    get_summary_all = function(predictionPath = self$predictionPath, 
                               selectionPath = self$selectionPath,
                               ignoreTimeoutsOnVBS=FALSE, 
                               method_numbers,
                               top_selections, 
                               median = FALSE,
                               delta = 0 , alpha = 0 , delta_prime= 0){
      runtime = data.frame(matrix(nrow = 0, ncol = 7))
      mcp = data.frame(matrix(nrow = 0, ncol = 7))
      par10 = data.frame(matrix(nrow = 0, ncol = 7))
      success = data.frame(matrix(nrow = 0, ncol = 7))
      for(method_number in method_numbers){
        for(top_selection in top_selections){
          if(method_number == 0){
            self$selection_based_on_SE(predictionPath = predictionPath, 
                                       saveTo = selectionPath, 
                                       method_number = method_number, 
                                       top_selection = top_selection,
                                       ignoreTimeoutsOnVBS = ignoreTimeoutsOnVBS)
            
          } else{
            self$selection_based_on_SE(predictionPath = predictionPath, 
                                       saveTo = selectionPath, 
                                       method_number = method_number, 
                                       top_selection = top_selection,
                                       ignoreTimeoutsOnVBS = ignoreTimeoutsOnVBS, 
                                       orderBy = "pred-se",
                                       delta = delta,
                                       delta_prime = delta_prime,
                                       alpha = alpha)
          }
          summary = self$get_summary_selection_result(selectionPath = selectionPath, ignoreTimeouts = ignoreTimeoutsOnVBS, 
                                                      method_number = method_number, median = median)
          runtime = rbind(runtime, summary[[1]])
          mcp = rbind(mcp, summary[[2]])
          par10 = rbind(par10, summary[[3]])
          success = rbind(success, summary[[4]])
          print(success)
        }
      }
      retval = list(runtime,mcp,par10,success)
      return(retval)
    },
    
    actual_error = function(predictionPath = self$predictionPath){
      predictionPath = paste0(predictionPath,"preds/") 
      solvers = self$sequentialData$solvers
      predictionCsvs = list.files(predictionPath, full.names = TRUE)
      results = data.frame(matrix(nrow = 0, ncol = 7))
      for(solver in solvers){
        for(csv in predictionCsvs){
          prediction = read.csv(csv)
          prediction = prediction[which(prediction$Solver == solver),]
          prediction$TrueError = prediction$ActualRuntime - prediction$PredictedRuntime
          results = rbind(results, prediction)
        }
      }
      return(results)
    }, 

    # Function to calculate the overlapping area
    overlap_area = function(preds) {
      # Calculate the intersection point
      preds$overlap_area = NA  # Initialize a new column for overlap_area
      preds$c = NA  
      min_pred = preds[which(preds$PredictedRuntime == min(preds$PredictedRuntime)),]
      for(i in 1:nrow(preds)){
        dist <- preds[i, ]
        if(dist$Prediction_StandardError != 0){
          c = ((dist$PredictedRuntime * (min_pred$Prediction_StandardError^2)) - (dist$Prediction_StandardError * ((min_pred$PredictedRuntime * dist$Prediction_StandardError) + min_pred$Prediction_StandardError * sqrt((min_pred$PredictedRuntime - dist$PredictedRuntime)^2 + (2 * (min_pred$Prediction_StandardError^2 - dist$Prediction_StandardError^2) * log(min_pred$Prediction_StandardError / dist$Prediction_StandardError)))))) / (min_pred$Prediction_StandardError^2 - dist$Prediction_StandardError^2)
          preds$c[i] = c
          
          # Calculate the overlapping area
          overlapping_area = 1 - pnorm(c, mean = min_pred$PredictedRuntime, sd = min_pred$Prediction_StandardError) + pnorm(c, mean = dist$PredictedRuntime, sd = dist$Prediction_StandardError)
          preds$overlap_area[i] = overlapping_area
        } else{
          preds$c[i] = preds$PredictedRuntime[i]
          preds$overlap_area[i] = NaN
        }
      }
      return(preds)
    },
    
    plot_overlap_area = function(preds){
      instanceName = preds$InstanceName[1]
      algorithms = preds$Solver

      min_pred = preds[which(preds$PredictedRuntime == min(preds$PredictedRuntime)),]
      # Assuming 'dists' and min_pred is a list of distributions with 'mean', 'sd', 
      preds = self$overlap_area(preds)
      colors <- rainbow(nrow(preds)+2)
      # Assuming 'dists' is a list of distributions with 'mean', 'sd', 'c', and 'overlap_area' calculated
      # And 'min_pred' is the distribution with the minimum mean.
      # Create the base plot

      # Data frame to store vertical line info
      vlines_data <- data.frame(xintercept = numeric(), dist_name = character())
      actual_data <- data.frame(xintercept = numeric(), dist_name = character())
      vbs = preds[which(preds$ActualRuntime == min(preds$ActualRuntime)),][1,]
      actual_data = rbind(actual_data, data.frame(xintercept = vbs$ActualRuntime, dist_name = paste0(vbs$Solver,"_VBS_truth")))
      actual_data = rbind(actual_data, data.frame(xintercept = min_pred$ActualRuntime, dist_name = paste0(min_pred$Solver,"_minPred_truth")))
      p <- ggplot() + xlim(min_pred$PredictedRuntime - 5 * min_pred$Prediction_StandardError, min_pred$PredictedRuntime + 5 * min_pred$Prediction_StandardError)

      # Loop through each distribution and add it to the plot with its own color
      for(i in 1:nrow(preds)) {
        dist_name <- preds[i,]$Solver
        print(dist_name)
        dist <- preds[i,]
        
        # Add the distribution curve to the plot
        if(dist$Prediction_StandardError != 0){
          # Create a data frame for the current distribution
          data_dist <- data.frame(x = seq(min_pred$PredictedRuntime - 5 * min_pred$Prediction_StandardError, min_pred$PredictedRuntime + 5 * min_pred$Prediction_StandardError, length.out = 100))
          data_dist$y <- dnorm(data_dist$x, mean = dist$PredictedRuntime, sd = dist$Prediction_StandardError)
          data_dist$dist_name <- dist_name  # Add a column for the distribution name
        
          p <- p + geom_line(data = data_dist, aes(x = x, y = y, color = dist_name), linewidth = 1)
        } else {
          # Collect data for vertical lines
          vlines_data <- rbind(vlines_data, data.frame(xintercept = dist$PredictedRuntime, dist_name = dist_name))
        } 
      }

      # Add all vertical lines to the plot at once
      if(nrow(vlines_data) > 0){
          p <- p + geom_vline(data = vlines_data, aes(xintercept = xintercept, color = dist_name), size = 1)
      }
      if(nrow(actual_data) > 0){
          p <- p + geom_vline(data = actual_data, aes(xintercept = xintercept, color = dist_name), size = 1)
      }
      # Define the colors for the lines
      p <- p + scale_color_manual(values = setNames(colors, preds$Algorithm))

      # Shade the overlap area and annotate
      for(i in 1:nrow(preds)) {
        dist <- preds[i,]
        # Only shade if it's not the distribution with the minimum mean
        if (dist$PredictedRuntime != min_pred$PredictedRuntime) {
          # Determine the direction to shade based on the mean
          if (dist$PredictedRuntime > min_pred$PredictedRuntime) {
            left_end <- dist$c
            right_end <- dist$PredictedRuntime + 3 * dist$Prediction_StandardError  # assuming within 3 SDs for visualization
          } else {
            left_end <- dist$PredictedRuntime - 3 * dist$Prediction_StandardError  # assuming within 3 SDs for visualization
            right_end <- dist$c
          }
          
          # Annotate the overlap area
          p <- p + annotate("text", x = dist$c, y = dnorm(dist$c, dist$PredictedRuntime, dist$Prediction_StandardError), 
                            label = sprintf("Area: %.4f", dist$overlap_area), 
                            hjust = 0.5, vjust = 1.5, size = 7)
        }
      }

      # Add labels and theming
      p <- p + labs(title = "Distribution Curves", x = "Value", y = "Density") +
              theme_minimal() +
              theme(plot.title = element_text(size = 30),     
                    axis.title = element_text(size = 20),     
                    axis.text = element_text(size = 12)) +
              guides(color = guide_legend(title = "Distribution"))

      return(p)
    },

    per_instance_overlapping_plot_resulting_runtime = function(){
      instances = self$get_all_preds()$InstanceName
      preds = self$get_all_preds()
      SEs = self$get_all_SEs()

      # for(ins in instances){
      ins = instances[1]
      pred = preds[which(preds$InstanceName == ins),]
      SE = SEs[which(SEs$InstanceName == ins),]
      SE = melt(SE, id.vars = "InstanceName")
      pred = melt(pred, id.vars = "InstanceName")
      colnames(SE) = c("InstanceName","Algorithm","sd")
      colnames(pred) = c("InstanceName","Algorithm","mean")
      df <- merge(pred, SE, by = c("InstanceName", "Algorithm"))

      p = self$plot_overlap_area(df)
      return(df)
      # }
    },
    
    kl_divergence_normal = function(mean_P, sd_P, mean_Q, sd_Q) {
      # Ensure standard deviations are positive
      if (sd_P <= 0 || sd_Q <= 0) {
        print("Standard deviations must be positive")
        return(NA)
      }

      # Calculate KL divergence using the formula
      kl_div <- log(sd_Q / sd_P) + ((sd_P^2 + (mean_P - mean_Q)^2) / (2 * sd_Q^2)) - 0.5
  
      return(kl_div)
    }, 

    # Calculate the mixture PDF (assuming equal weights)
    mixture_pdf = function(x, means, sds) {
      n = length(means)
      pdf = numeric(length(x))
      for (j in 1:n) {
        pdf = pdf + (1 / n) * dnorm(x, mean = means[j], sd = sds[j])
      }
      return(pdf)
    },

    pdf_difference = function(x, means1, sds1, means2, sds2) {
      pdf1 = self$mixture_pdf(x, means1, sds1)
      pdf2 = self$mixture_pdf(x, means2, sds2)
      return(pdf1 - pdf2)
    },

    # Define the CDF of the mixture of Gaussians
    cdf_mixture = function(x, means, sds) {
      sum_cdf = 0
      n = length(means)
      for(j in 1:n) {
        sum_cdf = sum_cdf + (1 / n) * pnorm(x, mean = means[j], sd = sds[j])
      }
      return(sum_cdf)
    }, 

    find_quantile_mixture = function(p, means, sds, lower = 0, upper = 5000) {
      # Function to find the root of F(x) - p
      fn <- function(x) self$cdf_mixture(x, means, sds) - p
      # Use uniroot to solve for x
      result <- uniroot(fn, lower = lower, upper = upper, tol = .Machine$double.eps^0.25)$root
      return(result)
    },

    # A simple scanning function to find sign-changing intervals
    find_sign_changes =function(means1, sds1, means2, sds2, from, to, steps = 100000) {
      x_values <- seq(from, to, length.out = steps)
      diff_values <- sapply(x_values, function(x) self$pdf_difference(x, means1, sds1, means2, sds2))
      sign_changes <- which(diff(sign(diff_values)) != 0)
      intervals <- x_values[sign_changes]
      return(intervals)
    },

    # Apply root finding within identified intervals
    find_roots = function(intervals, means1, sds1, means2, sds2) {
      roots <- numeric(length(intervals))
      for (i in seq_along(intervals)) {
        if (i < length(intervals)) {
          tryCatch({
            root <- uniroot(self$pdf_difference, lower = intervals[i], upper = intervals[i+1], means1 = means1, sds1 = sds1, means2 = means2, sds2 = sds2)$root
            roots[i] <- root
          }, error = function(e) {})
        }
      }
      return(na.omit(roots))
    }
  )
)