#!/bin/R
#' @author Paul Deveau <paul.deveau at curie.fr>
#' @author Pierre Gestraud

#' @title GraphLab
#'
#' @description Statuses for the gTag can be one of complete, ongoing, undoc (umented), unknown
#' @return \code{GraphLab} returns a list of with following elements: Functions (list of functions called by this function), interaction
#' (interaction matrix), and status (string with value corresponding to the gTag inside function corpus)
#' @param path : path to the R/ folder with scripts
#' @examples path<-system.file("extdata", package = "DevGRaph")
#' G<-GraphLab(path)
#' print(G$interaction)
#' print(G$status)
#' @importFrom utils getAnywhere getParseData
#' @export

GraphLab <- function(path = ""){  
  #'gTag complete
  
  ## is path a package directory or a "normal" directory with R scripts ?
  
  ### List all files of directory - or package
  file.list <- base::list.files(path = path,pattern = ".R$", full.names = TRUE)
  
  tmp_env <- new.env()
  
  lapply(file.list, sys.source, envir = tmp_env, keep.source = TRUE)
  
  allFunc <- eapply(tmp_env, function(x){
    # if (is.function(x)){
    #dput(x, file = file.path(tempdir(), "foo"))
    #d <- getParseData(parse(file.path(tempdir(), "foo")))
    d<-getParseData(x = parse(text = deparse(x), keep.source = TRUE))
    d <-d[d[,"token"] == "SYMBOL_FUNCTION_CALL",]
    if (!is.null(nrow(d))) d[, "pkg"] <- unlist(lapply(d[,"text"], function(f){
      found <- getAnywhere(f)$where
      found <- gsub("package:", "", found)
      found <- gsub("namespace:", "", found)
      found <- paste(unique(found), collapse = "|")
    }))
    #} else d <- NULL
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
  ### Option: create a gTag section with the following possibilities: complete, undoc, ongoing, unknown
  
  comms<-list()
  for(i in file.list){
    comms<-c(comms,get_comments(i))
  }
  status<-data.frame(func = names(allFunc), 
                     status = sapply(names(allFunc),FUN = function(z){
                       comments<-comms[z]
                       bin<-grepl(pattern = "#'gTag",x = comments,ignore.case = FALSE,fixed = TRUE)
                       if(sum(bin)){
                         char<-comments[bin]
                         if(grepl(pattern = "complete",x = char,ignore.case = TRUE)){
                           return("complete")
                         }else if(grepl(pattern = "undoc",x = char,ignore.case = TRUE)){
                           return("undocumented")
                         }else if(grepl(pattern = "ongoing",x = char,ignore.case = TRUE)){
                           return("ongoing")
                         }
                         else{
                           return("unknown")
                         }
                       }
                       else{
                         return("unknown")
                       }
                     })
  )
  result<-list(Functions = allFunc, interaction = interaction_matrix,status = status)
  
}


#' interact
#' 
#' Creates an interaction matrix for function
#' A 1 is present in column i, row j if i calls j
#' @param allFunc data generated inside of GraphLab
#' @param functions character vector with the name of all the functions from the pseudo-package
#' @param i position of the function to be tested inside functions
interact<-function(allFunc,functions,i = 1){
  #'gTag complete
  z<-functions[i]
  if(length(allFunc[[z]][["text"]])){
    return(as.numeric(functions %in% allFunc[[z]][["text"]]))
  }
  else{
    return(rep(0,times = length(functions)))
  }
}

#'PlotGraphLab
#'
#'Creates an interaction plot based on the output of GraphLab for a given function
#' @param GraphLab Output of the GraphLab function for the whole folder
#' @param func The function of interest for which the interaction graph should be plotted
#' @param filterOut name of packages from which the functions should be ignored. By default: base & utils
#' @param arrow_curv Curvature used for arrows showing non recursive calls (default: -0.2)
#' @param color Color of the outer box (default: black)
#' @param dictionnary A list with names the tags to use, and value the corresponding color. 
#' "unknown" is used for all tags that are not recognised and will be in light grey by default.
#' @export
#' @examples 
#' G<-GraphLab(system.file("extdata", "", package = "DevGRaph"))
#' # Same as DevGraphLab in this example:
#' PlotGraphLab(GraphLab = G, func = "Start")
#' # Graph for \code{progeny} function only
#' PlotGraphLab(GraphLab = G,func = "progeny")
#' # Ignoring ggplot2 imports:
#' PlotGraphLab(GraphLab = G,func = "Start", filterOut = "ggplot2")
#' @importFrom ggplot2 ggplot annotate geom_curve theme_void

PlotGraphLab <- function(GraphLab,func,filterOut = c("base","utils"),
                         arrow_curv = -0.2,color = "black",dictionnary = "default"){
  #'gTag : ongoing
  ### get interaction matrix and status for each function
  if(length(dictionnary)==1 && dictionnary=="default"){
    dictionnary<-c(complete = "green",ongoing = "orange",undocumented = "red",unknown = "lightgrey")
  }
  functions<-row.names(GraphLab$interaction)
  
  timeline<-extract_timeline(interact = GraphLab$interaction,
                             func = func,
                             time = 1)
  
  
  if(nrow(timeline)>1){ ### function has progeny
    #print(timeline)
    
    y<-0
    
    time<-timeline$timeline
    if(length(time)-1){
      for(i in 2:length(time)){
        y<-c(y,sum(time[i]==time[1:(i-1)]))
      }
    }
    #print(y)
    
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
                                            y2 = 0,
                                            func = z[2],
                                            #curvature = 0,
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
                                            #curvature = arrow_curv,
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
        #print(fun)
        arrow_data$x2[m]<-arrow_data$x2[min(m)]
        arrow_data$y2[m]<-arrow_data$y2[min(m)]
        
      }
      
    }
    
    ### check that start and end are not the same and add a little noise
    circular_test<-arrow_data$x1 == arrow_data$x2 & arrow_data$y1 == arrow_data$y2
    m<-which(circular_test)
    #arrow_data$x2[m]<-arrow_data$x2[m]+0.1
    arrow_data$y2[m]<-arrow_data$y2[m]+0.2
    arrow_data$text[m]<-FALSE
    #arrow_data$curvature[m]<-10
    
    sub<-arrow_data$text
    
    ### Avoid skipped values of x  
    L1<-length(na.omit(unique(arrow_data$x1)))
    while(sum(unique(na.omit(arrow_data$x1)) %in% 1:L1)<L1){ ### take first NA into account
      k<-min(which(!(1:L1 %in% unique(arrow_data$x1))))
      arrow_data$x1[arrow_data$x1>k]<-arrow_data$x1[arrow_data$x1>k]-1
      arrow_data$x2[arrow_data$x2>k]<-arrow_data$x2[arrow_data$x2>k]-1
      
    }
    
    arrow_data$x1<-arrow_data$x1+0.9
    arrow_data$x2[m]<-arrow_data$x2[m]+0.8
    
    arrow_data$y1<-arrow_data$y1-0.5
    
    #print(arrow_data)
    
    g<-ggplot2::ggplot(data = arrow_data[which(!circular_test),],
                       ggplot2::aes_string(x = "x1",
                                           xend = "x2",
                                           y = "y1",
                                           yend = "y2"
                       ))+
      ggplot2::geom_curve(curvature = I(arrow_curv),
                          arrow = ggplot2::arrow(length = ggplot2::unit(0.03, "npc"))
      )+
      ggplot2::annotate(geom = "text", ### Function title
                        x=arrow_data$x2[sub],
                        y=arrow_data$y2[sub]+0.05,
                        label = arrow_data$func[sub],
                        hjust = 0,
                        fontface = "bold"
      )+ggplot2::theme_void()
    if(sum(circular_test,na.rm = TRUE)){
      g<-g+ggplot2::geom_curve(data =arrow_data[m,],
                               curvature = I(1),
                               arrow = ggplot2::arrow(length = ggplot2::unit(0.03, "npc"))
      )
    }
    #xlim(c(0.5,max(arrow_data$x2)+1))+
    #ylim(c(-1,max(arrow_data$y2)+0.1))
    ###Add color from status
    
    arrow_data$status<-sapply(X = arrow_data$func, FUN = function(z){
      GraphLab$status$status[as.character(GraphLab$status$func) == as.character(z)]
    }
    )
    
    arrow_data$color<-dict(x = arrow_data$status,pattern = names(dictionnary),replace = as.character(dictionnary))
    # arrow_data$color<-sapply(X = arrow_data$status,FUN = function(z){
    #   if(z == "complete"){
    #     return("green")
    #   }else if(z=="ongoing"){
    #     return("orange")
    #   }else if(z=="undocumented"){
    #     return("red")
    #   }else{
    #     return("lightgrey")
    #   }
    # })
    #### Add functions from other packages
    AnnexCalls<-list()
    m<-0
    for(fun in unique(arrow_data$func)){
      AnnexCalls[[fun]]<-unique(GraphLab$Functions[[fun]][!(GraphLab$Functions[[fun]]$pkg %in% filterOut | GraphLab$Functions[[fun]]$text %in% unique(arrow_data$func)),
                                                          c("text","pkg")])
      m<-max(m,nrow(AnnexCalls[[fun]]))
    }
    m<-m+1
    for(fun in unique(arrow_data$func)){
      
      if(nrow(AnnexCalls[[fun]])){
        g<-g+
          ggplot2::annotate(geom = "text",
                            x = arrow_data$x2[arrow_data$func == fun & arrow_data$text] + 0*(1:nrow(AnnexCalls[[fun]])), ## trick to have same length vectors
                            y = arrow_data$y2[arrow_data$func == fun & arrow_data$text] - 0.9*(1:nrow(AnnexCalls[[fun]]))/m,
                            label = paste(AnnexCalls[[fun]]$pkg,AnnexCalls[[fun]]$text,sep= "::"),
                            #color = AnnexCalls[[fun]]$pkg,
                            hjust = 0)+
          ggplot2::annotate(geom = "rect",
                            xmin = arrow_data$x2[arrow_data$func == fun & arrow_data$text],
                            xmax = arrow_data$x2[arrow_data$func == fun & arrow_data$text]+0.9,
                            ymin = arrow_data$y2[arrow_data$func == fun & arrow_data$text] - 0.9,
                            ymax =  arrow_data$y2[arrow_data$func == fun & arrow_data$text]+0.1,
                            fill =  arrow_data$color[arrow_data$func == fun][1],
                            color = I(color),
                            alpha = 0.2
          )
        
      }
      else{
        g<-g+ggplot2::annotate(geom = "rect",
                               xmin = arrow_data$x2[arrow_data$func == fun & arrow_data$text],
                               xmax = arrow_data$x2[arrow_data$func == fun & arrow_data$text]+0.9,
                               ymin = arrow_data$y2[arrow_data$func == fun & arrow_data$text] - 0.9,
                               ymax =  arrow_data$y2[arrow_data$func == fun & arrow_data$text]+0.1,
                               fill =  arrow_data$color[arrow_data$func == fun][1],
                               color = I(color),
                               alpha = 0.2
        )
      }
    }
    return(g)
  }
  else{### Function is alone, but still should be plotted with its dependancies
    status<-GraphLab$status$status[as.character(GraphLab$status$func) == func]
    # fill_color<-sapply(X = status,FUN = function(z){
    #   if(z == "complete"){
    #     return("green")
    #   }else if(z=="ongoing"){
    #     return("orange")
    #   }else if(z=="undocumented"){
    #     return("red")
    #   }else{
    #     return("lightgrey")
    #   }
    # })
    fill_color<-dict(x = status,pattern = names(dictionnary),replace = as.character(dictionnary))
    AnnexCalls<-unique(GraphLab$Functions[[func]][!(GraphLab$Functions[[func]]$pkg %in% filterOut ),
                                                  c("text","pkg")])
    m<-nrow(AnnexCalls)
    g<-ggplot(x = 1, y = 1,xlim = c(1,2),
              ylim = c(0,1))+annotate(geom = "text",
                                      x = 1,
                                      y = 1,
                                      fontface = "bold",
                                      hjust = 0,
                                      label = func
              )+
      annotate(geom = "rect",
               xmin = 1,
               xmax = 1.9,
               ymin = 0,
               ymax = 1.1,
               fill =  fill_color,
               color = I(color),
               alpha = 0.2
      )+theme_void()
    if(!is.null(m) && m>0){
      Y<-(1:m)/(m+1)
      g<-g+annotate(geom = "text",
                    x = 1,
                    y = 1-Y,
                    hjust = 0,
                    label = paste(AnnexCalls$pkg,AnnexCalls$text,sep ="::")
      )
    }
    return(g) 
  }
}


#' Extract calling timeline
#' 
#' Extract calling timeline 
#' @param interact the interaction matrix create by interact function
#' @param func function of interest for which the timeline should be created
#' @param time the iterative time
#' @param calledBy the function which calls func
extract_timeline<-function(interact,func,time = 1 ,calledBy = "NA"){
  #'gTag : complete
  
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

#' DevGraphLab
#' 
#' Plots graph of interaction for all functions in the package
#' @param path Path to the folder with all R scripts for the package
#' @param  filterOut character vector of the packages to ignore in the graph
#' @param ... parameters passed to PlotGraphLab, such as dictionnary or arrow_curv.
#' @importFrom gridExtra grid.arrange
#' @return \code{DevGraphLab} returns a \code{ggplot2} graph if there is only one cluster of functions or a \code{gridExtra} object otherwise.
#' @examples 
#' path<-system.file("extdata", package = "DevGRaph")
#' DevGraphLab(path)
#' @export
DevGraphLab <- function(path,filterOut = c("base","utils"),...){
  #'gTag undoc
  Graph <- GraphLab(path = path)
  ### Should find the number of independant components in the package from the graph
  ### and return 1 plot for each component
  
  Masters<-find_clusters(Graph)
  if(length(Masters)==1){
    return(PlotGraphLab(GraphLab = Graph,
                        func = Masters,
                        filterOut =  filterOut,
                        ...))
  }
  else{
    n<-floor(sqrt(length(Masters)))+1
    
    plots<-list()
    for(i in 1:length(Masters)){
      plots[[i]]<-PlotGraphLab(GraphLab = Graph,
                               func = Masters[i],
                               filterOut =  filterOut,...)
    }
    
    return(gridExtra::grid.arrange(grobs = plots,ncol = n))
  }
}

#' The classic function to coerce to numeric
#' @param z the vector to be coerced
cnum<-function(z){
  #'gTag complete
  as.numeric(as.character(z))
}

#' Extract comments from functions
#' 
#' @param filename : filenames from which we should extract comments
get_comments <- function (filename) {
  #'gTag undoc
  ### from http://stackoverflow.com/questions/32651414/extract-comments-from-r-source-files-keep-function-in-which-they-occurs
  is_assign = function (expr)
    as.character(expr) %in% c('<-', '<<-', '=', 'assign')
  
  is_function = function (expr)
    is.call(expr) && is_assign(expr[[1]]) && is.call(expr[[3]]) && expr[[3]][[1]] == quote(`function`)
  
  source = parse(filename, keep.source = TRUE)
  functions = Filter(is_function, source)
  fun_names = as.character(lapply(functions, `[[`, 2))
  return(setNames(lapply(attr(functions, 'srcref'), grep,
                         pattern = '^\\s*#', value = TRUE), fun_names)
  )
}

#' Finds clusters of interactions
#' 
#' @param GraphLab output from GraphLab function
#' 
find_clusters<-function(GraphLab){
  #'gTag complete
  ### Get all functions that are called by no-one
  return(row.names(GraphLab$interaction)[rowSums(GraphLab$interaction)==0])
}


#' Shows all imported functions
#' 
#'  @param GraphLab output from GraphLab function
#'  @param onlyMissingImports show function that are not taken into account in the imports
#'  @param filterOut packages from which functions should not be shown
showImports<-function(GraphLab, onlyMissingImports = FALSE,filterOut = "base"){
  #'gTag undoc
  if(onlyMissingImports){
    miss<-character()
    for(i in GraphLab$Functions){
      miss<-c(miss,i[ i$pkg=="", "text"])
    }
    return(miss)
  }
  else{
    
    df<-data.frame()
    for(i in GraphLab$Functions){
      
      if(nrow(i)){ ### skip if function has no row (pkg column missing)
        df<-rbind(df,i[!(i$pkg %in% filterOut) ,c("text","pkg")])
      }
    }
    df<-unique(df)
    df<-df[order(df$pkg,df$text,decreasing = FALSE),]
    return(df)
  }
  
}

#' Function to match comments and colors
#' 
#' @param x The comments to change
#' @param pattern the value to change
#' @param replace the value which should be replaced
dict <- function(x, pattern, replace) {
  x<-as.character(x)
  if(sum(pattern=="unknown")){
    w<-which(pattern=="unknown")
    pattern<-c(pattern[-w],"unknown")
    replace<-c(replace[-w],replace[w])
    x0<-x %in% pattern[-length(pattern)]
    if(sum(!x0)){
      x[!x0]<-tail(replace,1)
    }
    for(i in 1:(length(replace)-1)){
      sub<-x==pattern[i]
      if(sum(sub)){
        x[sub]<-replace[i]
      }
    }
  }
  else{
    x0<-x %in% pattern
    if(sum(!x0)){
      x[!x0]<-"lightgrey"
    }
    for(i in 1:(length(replace))){
      x[x==pattern[i]]<-replace[i]
    }
  }
  return(x)
}