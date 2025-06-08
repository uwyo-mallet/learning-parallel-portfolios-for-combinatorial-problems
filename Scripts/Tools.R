#Haniye Kashgarani 
#Feb 8th, 2022
#More Organized code
packages <- c("checkmate", "R6", "stringr", "aslib", "llama", "matrixStats", "mlr3", "mlr3pipelines", "mlr3learners","mlr3extralearners",
              "mlr3tuning", "parallel", "parallelMap", "plotly", "RWeka", "methods", "ParamHelpers", "batchtools", "BBmisc", "tidyr", 
              "tibble", "ggplot2", "stringr", "reshape", "reshape2", "gtools", "fitdistrplus", "emojifont")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

Tools <- R6Class(
  classname = "Tools",
  public = list(
    #set working directory
    set_wd = function(path, cluster = FALSE){
      if(Sys.info()['sysname']!="Linux"){
        path = paste0("C:/Users/hnyk9/Thesis/",path)
        if(dir.exists(path)){
          setwd(path)
        } else {print(paste0("Directory '",path,"' doesn't exits!"))}
      } else{
        path = paste0("~/Documents/",path,"/")
        if(cluster){
          path = paste0("/gscratch/hkashgar/",path,"/")
        }
        if(dir.exists(path)){
          setwd(path)
        } else {print(paste0("Directory '",path,"' doesn't exits!"))}
      }
      invisible(self)
    },
    
    create_directory_if_not_exists = function(directory_path) {
      if (!file.exists(directory_path)) {
        dir.create(directory_path, recursive = TRUE)
        cat("Directory created at:", directory_path, "\n")
      } else {
        cat("Directory already exists at:", directory_path, "\n")
      }
    },
    
    plot_hist_plotly = function(vector){
      plot_ly(
        x = vector,
        type = "histogram"
      )
    },
    #doesn't work
    plot_dens_plotly = function(dataframe){
      data_long <- gather(dataframe, core, overhead, X2:X32, factor_key=TRUE)
      data_long$core <- gsub("X","",data_long$core)
      data_long$core = as.factor(as.numeric(data_long$core))
      fig <- plot_ly(data = dataframe)
      if(ncol(dataframe>=1)){
        for(i in 1:ncol(dataframe)){
          dens = density(unlist(unname(dataframe[i])))
          fig = add_trace(fig,x = ~dens$x, y = ~dens$y,type = 'scatter',mode = 'lines', name = paste0(str_split(colnames(dataframe[i]),"X")[[1]][2],"cores"), fill = 'tozeroy')
        }
      }
      fig <- fig %>% layout(xaxis = list(title = 'Overhead Percentage'),
                            yaxis = list(title = 'Density'))
      fig
    }
    
    #move files
    
    #check if exist 
    
    #latex table
  )
)

