#!/bin/R

GraphLab<-function(path = ""){
  ### List all files of directory - or package
  
  file.list<-list.files(path = path,pattern = "*.R")
  tmp_env<-new.env()
  
  for(file in file.list){
    source(file = file)
  }
  
  env_items<-ls(envir = tmp_env) ### Does it take into account data too? (example : maps in ACSNMineR package)
  functions<-env_items[apply(X = env_items,
                   FUN = function(z) eval(parse(text = z))=="function"
              )
              ]
  
  ### Create interation matrix
  interaction_matrix<-matrix(0,
                             nrow = length(functions),
                             ncol = length(functions))
  for(func in functions){
      ### Is another function from  the function list called?
      ### Are you importing from another package? (then which? => devtool::check)
      
      
    }
  ### Get result from roxygen2 : missing descriptions, exported or not, etc.
  
}

PlotGraphLab<-function(GraphLab){
  ### get interaction matrix and status for each function
  
  
}