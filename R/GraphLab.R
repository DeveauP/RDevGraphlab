#!/bin/R

toto <- NULL

GraphLab <- function(path = ""){  
  #'gTag ongoing
  ## is path a package directory or a "normal" directory with R scripts ?
  
  ### List all files of directory - or package
  file.list <- base::list.files(path = path,pattern = ".R$", full.names = TRUE)
  
  tmp_env <- new.env()
  
  lapply(file.list, source, local = tmp_env)
  
  allFunc <- eapply(tmp_env, function(x){
    dput(x, file = file.path(tempdir(), "foo"))
    d <- getParseData(parse(file.path(tempdir(), "foo")))
    d <- subset(d, token == "SYMBOL_FUNCTION_CALL")
    if (nrow(d)) d[, "pkg"] <- gsub("package:", "", unlist(lapply(d[,"text"], function(f) paste(find(f), collapse = "|"))))
    return(d)
  })
  functions <- eapply(tmp_env, is.function)
  functions <- names(functions)[unlist(functions)]  
  
  ### Create interation matrix
  interaction_matrix <- matrix(0,
                               nrow = length(functions),
                               ncol = length(functions),
                               dimnames = list(functions, functions))
  for (i in 1:length(functions)) {
    ### Is another function from  the function list called?
    ### Storing vertically : 1 in i column j row means function i calls function j
    interaction_matrix[,i] <- interact(allFunc = allFunc,i = i,functions = functions)
  }
  
  ### Extract tags : requires other method (saving in tmp file erases comments)
  result<-list(Functions = allFunc, interaction = interaction_matrix)
  
}



interact<-function(allFunc,functions,i = 1){
  #'gTab uncommented
  z<-functions[i]
  if(length(allFunc[[z]][["text"]])){
    return(as.numeric(functions %in% allFunc[[z]][["text"]]))
  }
  else{
    return(rep(0,times = length(functions)))
  }
}


PlotGraphLab <- function(GraphLab,func){
  #'gTag : uncomplete
  ### get interaction matrix and status for each function
  functions<-row.names(GraphLab$interaction)
  timeline<-extract_timeline(interact = GraphLab$interaction,
                             func = func,
                             time = 1)
  print(timeline)
  
  
  
}

extract_timeline<-function(interact,func,time = 1 ,calledBy = "NA"){
  #'gTag : uncomplete
  
  print(paste("func:",func))
  print(paste("calledBy:", calledBy))
  
  result<-data.frame(timeline = time, 
                     func = func,
                     calledBy = calledBy)
  
  vec<-interact[,func]
  if(sum(vec)){ ### the function calls other functions
    for(i in names(vec)[as.logical(vec)]){
      if(i!=func){ ### recursive progamming
        result<-rbind(result,
                      extract_timeline(interact = interact,
                                       func = i,
                                       time = time +1,
                                       calledBy = func
                                       )
        )
      }
      else{
        result<-rbind(result,
                      data.frame(timeline = time,
                                 func = func,
                                 calledBy = func))
      }
    }
    
  }
  return(result)
}

DevGraphLab <- function(path){
  Graph <- GraphLab(path = path)
  #print(Graph)
  return(PlotGraphLab(GraphLab))
  
}

showTab <- function(allFunc, funcName){
  datatable(allFunc[[funcName]][, c("text", "pkg")], caption = funcName, rownames=FALSE)   
}

