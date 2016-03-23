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
                                          x2 = 1,
                                          y1 = NA,
                                          y2 = 1,
                                          func = z[2],
                                          curvature = 0,
                                          text = TRUE
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
                                          func = z[2],
                                          curvature = 0.5,
                                          text = TRUE
                        )
                        )
                      }
                    })
  arrow_data<-do.call(rbind.data.frame, arrow_data)
  arrow_data$y2<-cnum(arrow_data$y2)
  arrow_data$x2<-cnum(arrow_data$x2)
  
  
  ### unicity of points in arrow data
  for(fun in unique(arrow_data$func)){
    m<-which(arrow_data$func==fun)
    if(length(m)-1){
      print(fun)
      arrow_data$x2[m]<-arrow_data$x2[min(m)]
      arrow_data$y2[m]<-arrow_data$y2[min(m)]
      
    }
    
  }
  
  ### check that start and end are not the same and add a little noise
  m<-which(arrow_data$x1 == arrow_data$x2 & arrow_data$y1 == arrow_data$y2)
  #arrow_data$x2[m]<-arrow_data$x2[m]+0.1
  arrow_data$y2[m]<-arrow_data$y2[m]+0.2
  arrow_data$text[m]<-FALSE
  arrow_data$curvature[m]<-10
  
  sub<-arrow_data$text
  print(arrow_data)
  print(arrow_data[arrow_data$curvature>0,])
  g<-ggplot(data = arrow_data[arrow_data$curvature>0,],
            aes_string(x = "x1",
                       xend = "x2",
                       y = "y1",
                       yend = "y2",
                       curvature = "curvature"))+
    geom_curve(
      arrow = arrow(length = unit(0.03, "npc"))
    )+
    annotate(geom = "text",
             x=arrow_data$x2[sub],
             y=arrow_data$y2[sub]+0.05,
             label = arrow_data$func[sub]
    )+theme_void()+
    xlim(c(0,max(arrow_data$x2)+0.5))+
    ylim(c(-0.05,max(arrow_data$y2)+0.1))
  
  
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

cnum<-function(z){as.numeric(as.character(z))}