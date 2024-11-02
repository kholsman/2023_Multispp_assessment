#'
#'
#' get_ClimateNaiveBtarget.R
#' 
#' 

rset <- c(1)
# Single species
multispp_mode <- FALSE
for(rec_mode in rset){
  for(hcr_mode in c(3)){
    
    run_fut(
      ex_model    = modelIN,
      updateModel = FALSE,
      run_version = run_versionIN,
      proj_path  = file.path(run_fl),
      data_pathIN  = file.path(run_fl,"data/in"), 
      model_pathIN = DIR_main,
      run_path   = file.path(DIR_main,config$model_path),
      multispp   = multispp_mode,
      ctl_file   = ctl_0,  #asmnt2022_0A
      rec_num    = rec_mode,
      hcr_num    = hcr_mode,
      plotIT     = TRUE,
      copy2proj  = TRUE,
      est_modIN  = filenm,#assmnt_2022
      flnmIN     = filenm, #assmnt_2022
      overwrite  = TRUE,
      debug      = FALSE,
      skip_est   = FALSE,
      configIN   = config,
      DIR_mainIN   = DIR_main)
    
    # run_version = run_versionIN
    # proj_path  = file.path(run_fl)
    # run_path   = file.path(DIR_main,config$model_path)
    # rec_num    = rec_mode
    # hcr_num    = hcr_mode
    # est_modIN  = filenm
    # multispp   = multispp_mode
    # source(file.path(DIR_main,"R/sub_scripts/PLOT_CEATTLE_FUT_2023.R"))
    
  }
}


# Now for multispecies
multispp_mode <- TRUE

for(rec_mode in rset){
  for(hcr_mode in 3){
    
    
    run_fut(
      ex_model    = modelIN,
      updateModel = FALSE,
      run_version = run_versionIN,
      proj_path  = file.path(run_fl),
      data_pathIN  = file.path(run_fl,"data/in"), 
      model_pathIN = DIR_main,
      run_path   = file.path(DIR_main,config$model_path),
      multispp   = multispp_mode,
      rec_num    = rec_mode,
      hcr_num    = hcr_mode,
      plotIT     = TRUE,
      copy2proj  = TRUE,
      est_modIN    = filenm,#assmnt_2022
      flnmIN     = filenm, #assmnt_2022
      ctl_file   = ctl_2,  #asmnt2022_2A
      overwrite  = TRUE,
      debug      = FALSE,
      skip_est   = FALSE,
      configIN   = config,
      DIR_mainIN   = DIR_main)
    
    
      # run_version = run_versionIN
      # proj_path  = file.path(run_fl)
      # run_path   = file.path(DIR_main,config$model_path)
      # rec_num    = rec_mode
      # hcr_num    = hcr_mode
      # est_modIN  = filenm
      # multispp   = multispp_mode
      # source(file.path(DIR_main,"R/sub_scripts/PLOT_CEATTLE_FUT_2023.R"))
    
  }
}




tt      <- scan(file=file.path(ctl_main0,paste0(ctl_0,".ctl")),
                what=character(),sep="\n")
futfile <- tt[grep("futfile_name",tt)+1]
tt      <- scan(file.path(run_fl,"data/in",futfile),
                what=character(),sep="\n")
nscen   <- as.numeric(tt[grep("num_runs",tt)[1]+1])
st      <- grep("ncov_fut",tt)+3;ed<-st+nscen-1
mods    <- tt[st:ed]
for(i in 1:nscen)
  mods[i] <- strsplit(mods[i],"# | ")[[1]][3]
# mods[i]<-strsplit(mods,split=" ")[[i]][3]
nspp    <- 3


mods <- unlist(strsplit(gsub(" ","",tt[grep("Models",tt)+1]),"#"))
mods <- mods[-which(mods=="")]
mods <- mods[c(grep("mnhind",mods),
                           grep("126",mods),
                           grep("45",mods),
                           grep("85",mods))]
hind_n <- which(mods=="mnhind")
ssps <- unique((unlist(lapply(strsplit(mods[-hind_n],split="_"),"[[",2))))
gcms <- unique(unlist(lapply(strsplit(mods,split="_"),"[[",1)))
mods_df <- data.frame(
  Projection=mods, stringsAsFactors = F)%>%rowwise()%>%
  mutate(GCM      = strsplit(Projection,split="_")[[1]][1],
         Scenario = getScen(Projection))


#-------------------------------------
# GET BO VALUES --> B0_list
#-------------------------------------
hcrset <- 1.7
B0_list <- list()
r       <- 1
h       <- 3
hcrr    <- hcrset
m       <- 0

B0_list[["B0_0"]] <- getB0(mn     = file.path(run_fl,"runs",run_versionIN),
                           flname = filenm, 
                           modelname=  filenm, 
                           rec    = r, 
                           hvst   = h, 
                           mode   = 0,
                           hcr    = hcrr,
                           BplusYrs = thisYr + c(1,2))

B0_list[["B0_2"]] <- getB0(mn     = file.path(run_fl,"runs",run_versionIN),
                           flname = filenm, 
                           rec    = r, 
                           hvst   = h, 
                           mode   = 2,
                           hcr    = hcrr,
                           BplusYrs = thisYr + c(1,2))

