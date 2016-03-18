#!/bin/R

toto <- NULL

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
    
    aa<-PCA(r)    
    
    functions <- eapply(tmp_env, is.function)
    functions <- names(functions)[unlist(functions)]  
    
  ### Create interation matrix
  interaction_matrix <- matrix(0,
                             nrow = length(functions),
                             ncol = length(functions),
                             dimnames = list(functions, functions))
  for (i in 1:length(functions)) {
    ### Is another function from  the function list called?
    ### Are you importing from another package? (then which? => devtool::check)
    ### Storing vertically : 1 in i column j row means function i calls function j
    interaction_matrix[,i] <- sapply(X = functions, FUN = function(z){  
      ### body returns a vector => see if you call it more than once
      ### We want function to start with the name and finish with parenthesis => exlude function that could
      ### have partially identical names or items with same names as functions

      x <- unlist(strsplit(x = as.character(body(functions[i])), split =" "))
      a <- sum(grepl(pattern = paste("(\\(?)",z,'(\\()(.*)(\\))',sep=""),
               x =  x)
      )
      print(a)
      return(as.numeric(a > 0))
    })
  }
  ### Get result from roxygen2 : missing/incomplete descriptions, exported or not, etc.
  ### If from roxygen: check looks for Rd files, may not be of interest for scripts
  
}

PlotGraphLab <- function(GraphLab){
  ### get interaction matrix and status for each function
  
  
}

DevGraphLab <- function(path){
  Graph <- GraphLab(path = path)
  return(PlotGraphLab(GraphLab))
  
}

showTab <- function(allFunc, funcName){
    datatable(allFunc[[funcName]][, c("text", "pkg")], caption = funcName, rownames=FALSE)   
}

