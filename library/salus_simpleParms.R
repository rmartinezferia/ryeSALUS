createsalusbat <-
  function (wrkdir = "", resdir = "", sdb = "", wdb = "", xdb = "", 
            cdb, vars1 = "", freq1 = "", vars2 = "", freq2 = "", gdb = "salus.gdb.xml", 
            executable = "/mnt/research/bassolab/SalusModel/salus_gnu", 
            limitations = "-wn", msglevel = "debug", file1 = "out1.csv", 
            file2 = "out2.csv", msgfile = "out.log") {
    bat.args <- sapply(c("sdb", "wdb", "xdb", "cdb", "vars1", 
                         "freq1", "vars2", "wrkdir", "freq2", "msglevel", "file1", 
                         "file2", "msgfile"), function(x) {
                           obj <- get(x, envir = environment()
                                      )
                           if (obj != "") {
                             obj
                           }
                         })
    bat.args <- unlist(bat.args[!sapply(bat.args, is.null)])
    bat.args <- sapply(names(bat.args), function(x) {
      paste(x, bat.args[x], sep = "=")
    })
    bat.args <- paste(bat.args, collapse = " ")
    return(paste(executable, limitations, bat.args))
  }


# Run Salus 
salus <- function(options = " -wn",
                  outfile = "salus.out.Rdata",
                  executable = "C:\\SALUS\\Salus_20191106\\SalusC_64.exe",#"C:\\Users\\mart2225\\Dropbox\\SALUS\\SalusWorkingWin\\SalusC_64.exe",  #"/mnt/research/bassolab/SalusModel/salus_gnu_2019_05_31",
                  wrkdir= "calibration",
                  xdb="calibration.xdb.xml",
                  cdb="crops.cdb.xml",
                  verbose = TRUE,
                  freq1= "1",
                  file1 = "out.csv",
                  vars1 = "Title,layer:9:RWADL,layer:9:NIADL,layer:9:SW,SWAD,LWAD,RWAD,layer:9:ST,layer:9:BD,Rain,N_In,NOAD,NIAD,layer:9:C_Tot,C_Net,N_Net,N_Out,C_AtmoCO2,TMNA,TMXA,Rain,KRPP,N_Vol,OWAD,Year,Date,GWAD,CWAD,Biom,LAI,EPAD,ESAD,N_Plants,P_Plants,SWroot,SWtot,SoilNroot,SoilNtot,RootDep,NLCC,NitroFac,HeatFac,ColdFac,SWXD,EOAD,DrghtFac,DAP,C_CO2,SpeciesID,RcID,RID,layer:9:SWCN,cThrTime"
){
  
  #################################
  ### Run
  # #################################
  
  salusbat <- createsalusbat(wrkdir=wrkdir,
                             xdb=xdb,
                             cdb=cdb,
                             executable = executable,
                             msglevel="status",
                             vars1=vars1,
                             vars2="",
                             freq1=freq1,
                             freq2="",
                             file1=file1,
                             file2="",
                             limitations=options)
  
  if(grepl("mingw",version$os)) {
    
    shellscript <- paste0(paste0(salusbat, collapse = "\n"),collapse = "\n")
    if(verbose) cat(shellscript)
    system(shellscript, intern = !verbose)
    
  } else {
    
    shellscript <- paste0("#!/bin/bash -login\n", paste0(salusbat, collapse = "\n"),collapse = "\n")
    cat(shellscript, file = temp.file <- tempfile())
    system(paste("chmod 744", temp.file, ";", temp.file))
    
  }
  
  return(read.csv(file.path(wrkdir,file1)))
  
}


salus_simpleParms <- function(parms = list(),
                              options = "",
                              outfile = "salus.out.Rdata",
                              executable = "C:\\SALUS\\Salus_20191106\\SalusC_64.exe",#"C:\\Users\\mart2225\\Dropbox\\SALUS\\SalusWorkingWin\\SalusC_64.exe",  #"/mnt/research/bassolab/SalusModel/salus_gnu_2019_05_31",
                              wrkdir="calibration",
                              xdb="calibration.xdb.xml",
                              cdb="crops_swg.cdb.xml",
                              overwrite_cdb = TRUE,
                              verbose = FALSE,
                              freq1="1",
                              vars1 = "Title,layer:9:RWADL,layer:9:NIADL,layer:9:SW,SWAD,LWAD,RWAD,layer:9:ST,layer:9:BD,Rain,N_In,NOAD,NIAD,layer:9:C_Tot,C_Net,N_Net,N_Out,C_AtmoCO2,TMNA,TMXA,Rain,KRPP,N_Vol,OWAD,Year,Date,GWAD,CWAD,Biom,LAI,EPAD,ESAD,N_Plants,P_Plants,SWroot,SWtot,SoilNroot,SoilNtot,RootDep,NLCC,NitroFac,HeatFac,ColdFac,SWXD,EOAD,DrghtFac,DAP,C_CO2,SpeciesID,RcID,RID,layer:9:SWCN,cThrTime"
){
  
  # #####################################
  # ###Adjust crop parameter
  # ######################################
  
  if(length(parms) > 0){
    CDB.node <- xmlParse(paste0("assets/",cdb))
    for(SpeciesID in names(parms)){p <- parms[[SpeciesID]];
    xpathSApply(CDB.node,"//Simple_Crop/Species",function(x){if(xmlAttrs(x)["SpeciesID"]==SpeciesID){xmlAttrs(x)[names(p)] <- p}})}
    
    if(overwrite_cdb){
      saveXML(CDB.node, paste0(wrkdir,"/",cdb))
    }  else {
      cdb <- tempfile()
      saveXML(CDB.node, cdb)
    }
  }
  
  
  #################################
  ### Run
  # #################################
  
  file1 <- tempfile()
  salusbat <- createsalusbat(wrkdir=wrkdir,
                             xdb=xdb,
                             cdb=cdb,
                             executable = executable,
                             msglevel="status",
                             vars1=vars1,
                             vars2="",
                             freq1=freq1,
                             freq2="",
                             file1=file1,
                             file2="",
                             limitations=options)
  
  
  if(grepl("mingw",version$os)) {
    
    shellscript <- paste0(paste0(salusbat, collapse = "\n"),collapse = "\n")
    if(verbose) cat(shellscript)
    system(shellscript, intern = !verbose)
    
  } else {
    
    shellscript <- paste0("#!/bin/bash -login\n", paste0(salusbat, collapse = "\n"),collapse = "\n")
    cat(shellscript, file = temp.file <- tempfile())
    system(paste("chmod 744", temp.file, ";", temp.file))
    
  }
  
  out <- read.csv(file1)
  save("out",file = outfile)
  return(out)
  
}


NSE <- function(obs,pred){1 - (sum((obs - pred)^2)/sum((obs - mean(obs))^2))}

myOVP <- function(y,x) coef(lm(y~x))

regFit <- function(obs,pred,digits = 2) paste0("y = ",signif(coef(lm(obs~pred))[1],digits)," + ",signif(coef(lm(obs~pred))[2],digits),"x")

fitSummary <- function(data, units = "Mg/ha/yr", collapse = "\n", equation = TRUE){
  
  x <- data %>%
    select(obs,pred) %>%
    filter(!is.na(obs),!is.na(pred)) %>%
    summarise(NSE = paste0("NSE = ",round(NSE(obs,pred),2)),
              RMSE = paste0("RMSE = ",round(sqrt(mean((pred - obs)^2)),1)," ",units),
              #RRMSE = paste0("RRMSE = ",round(100*sqrt(mean((pred - obs)^2))/mean(obs)),"%"),
              REG = regFit(obs,pred))
  if(!equation){
    x %>%
      mutate(label = paste0(NSE,collapse,RMSE)) %>%
      select(-NSE,-RMSE,-REG)
  } else {
    x %>%
      mutate(label = paste0(NSE,collapse,RMSE,collapse,REG)) %>%
      select(-NSE,-RMSE,-REG)
  }
    
  
}
