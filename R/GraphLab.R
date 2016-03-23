#!/bin/R

toto <- NULL


# GraphLab
#'
#'@param path
#'@section gTag ongoing

GraphLab <- function(path = ""){  
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
  ### Use roxygen and extract from Rd file? : see https://developer.r-project.org/parseRd.pdf
  ### Option: create a gTag section with the following possibilities: complete, undocumented, ongoing
  
  
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


PlotGraphLab <- function(GraphLab,func,filterOut = c("base","utils")){
  #'gTag : uncomplete
  ### get interaction matrix and status for each function
  functions<-row.names(GraphLab$interaction)
  timeline<-extract_timeline(interact = GraphLab$interaction,
                             func = func,
                             time = 1)
  print(timeline)
  
  y<-1
  
  time<-timeline$timeline
  if(length(time)-1){
    for(i in 2:length(time)){
      y<-c(y,sum(time[i]==time[1:(i-1)]))
    }
  }
  print(y)
  
  timeline$y<-y
  arrow_data<-apply(X = timeline,
                    MARGIN = 1,
                    FUN = function(z){
                      #print(names(z))
                      ### z: time, func, calledBy, y
                      if(z[3]=="NA"){ 
                        return(data.frame(x1 = NA,
                                          x2 = 0,
                                          y1 = NA,
                                          y2 = 0,
                                          func = z[2]
                        ))
                      }
                      else{
                        #print(paste(z[2],",",z[3]))
                        caller<-min(which(timeline$func == z[3]))
                        #print(caller)
                        return(data.frame(x1 = timeline$time[caller],
                                          x2 = z[1],
                                          y1 = timeline$y[caller],
                                          y2 = z[4],
                                          func = z[2]
                        )
                        )
                      }
                    })
  arrow_data<-do.call(rbind.data.frame, arrow_data)
  
  
  print(arrow_data)
  g<-ggplot(data = arrow_data[-1,],
            aes_string(x = "x1",
                       xend = "x2",
                       y = "y1",
                       yend = "y2" ))+
    geom_curve(
      arrow = arrow(length = unit(0.03, "npc"))
    )+
    annotate(geom = "text",
             x = arrow_data$x2,
             y = arrow_data$y2,
             label = arrow_data$func
    )+theme_void()
  
  
  ### Issue with plot: if 2 segments starts from same point: one will be shifted to -1
  return(g)
  
  
}

extract_timeline<-function(interact,func,time = 1 ,calledBy = "NA"){
  #'gTag : uncomplete
  
  #print(paste("func:",func))
  #print(paste("calledBy:", calledBy))
  
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

