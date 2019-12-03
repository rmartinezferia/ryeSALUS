
state <- c("KS"#,
           #"NE","ND"
           )
d <- c(#"260",
       "280","300")
  
for(i in state){
  
  print(i)
  for(j in d){
    
    print(j)
    files <- list.files(paste0("regionalSim/",i), pattern = paste0(j,".csv"))
    df <- data.frame()
    
    for(k in 1:length(files)){
      
      df <- bind_rows(df, read.csv(paste0("regionalSim/",i,"/",files[k])))
    
      
      cat("\r ",i,"- ",j,": ", round(k/length(files),2)*100,"%")
      
      
    }
    
    saveRDS(df,paste0("regionalSim/",i,"_",j,"_raw.rds"))
    
  }
}

