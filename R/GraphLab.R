#!/bin/R

GraphLab<-function(path = ""){
  ### List all files of directory - or package
  
  file.list<-list.files(path = path,pattern = "*.R")
  tmp_env<-new.env()
  
  for(file in file.list){
    source(file = paste(path,file,sep ="/"),local = tmp_env,print.eval = TRUE)
  }

  env_items<-unlist(ls(envir = tmp_env)) ### Does it take into account data too? (example : maps in ACSNMineR package)
  functions<-env_items[sapply(X = env_items,
                             FUN = function(z){
                               e<-is.function(eval(parse(text = z)))
                             } 
                        )
  ]
  
  ### Create interation matrix
  interaction_matrix<-matrix(0,
                             nrow = length(functions),
                             ncol = length(functions))
  for(i in 1:length(functions)){
    ### Is another function from  the function list called?
    ### Are you importing from another package? (then which? => devtool::check)
    ### Storing vertically : 1 in i column j row means function i calls function j
    interaction_matrix[,i]<-sapply(X = functions, FUN = function(z){  
      ### body returns a vector => see if you call it more than once
      ### We want function to start with the name and finish with parenthesis => exlude function that could
      ### have partially identical names or items with same names as functions

      x<-unlist(strsplit(x = as.character(body(functions[i])), split =" "))
      a<-sum(grepl(pattern = paste("(\\(?)",z,'(\\()(.*)(\\))',sep=""),
               x =  x)
      )
      print(a)
      return(as.numeric(a>0))
    })
  }
  ### Get result from roxygen2 : missing/incomplete descriptions, exported or not, etc.
  ### If from roxygen: check looks for Rd files, may not be of interest for scripts
  
}

PlotGraphLab<-function(GraphLab){
  ### get interaction matrix and status for each function
  
  
}

DevGraphLab<-function(path){
  Graph<-GraphLab(path = path)
  return(PlotGraphLab(GraphLab))
  
}