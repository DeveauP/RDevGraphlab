Start<-function(){
  progeny()
  undocumented()
  complete()
  unknown()
  ongoing()
}
progeny<-function(){
  recursive(1)
  calls_ggplot2_function()
}

undocumented<-function(){
  #'gTag undoc
  return(NA)
}

complete<-function(){
   #'gTag complete
   return(NA)
}

unknown<-function(){
  return(NA)
  
}

ongoing<-function(){
  #'gTag ongoing
  return(NA)
}

recursive<-function(x){
  if(x == 1){
    return(recursive(i+1))
  }
  else(return(i))
}

calls_ggplot2_function<-function(){
  ggplot2::qplot(x = 1:10,y=1:10)
  
}

Outsider<-function(){
  stats::as.formula(1+1)
  rbind(1:5,2:6)
  
}