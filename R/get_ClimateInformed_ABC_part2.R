
#'
#'
#'
#'
#'get_ClimateInformed_ABC_part2
#' Kirstin Holsman
#' 2023
#' Assessment steps to getting CEATTLE BRPs
#'
#'
#'
#'
#'
if(1==2){
  #now skip
  rset <- c(1,4,5,6)
  rset <- c(1)
  # Single species
  for(rec_mode in rset){
    for(hcr_mode in 3){
      
      run_fut(
        ex_model    = modelIN,
        updateModel = FALSE,
        run_version = run_versionIN,
        proj_path  = file.path(run_fl),
        data_path  = file.path(run_fl,"data/in"), 
        model_path = file.path(run_fl,"model"),
        run_path   = file.path(DIR_main,config$model_path),
        multispp   = FALSE,
        ctl_file   = ctl_0,  #asmnt2023_0A
        rec_num    = rec_mode,
        hcr_num    = hcr_mode,
        plotIT     = TRUE,
        copy2proj  = TRUE,
        est_modIN  = filenm,#assmnt_2023
        flnmIN     = filenm, #assmnt_2023
        overwrite  = TRUE,
        debug      = FALSE,
        skip_est   = FALSE,
        configIN   = config,
        DIR_mainIN   = DIR_main)
      
      
    }
  }
  
  
  # Now for multispecies
  for(rec_mode in rset){
    for(hcr_mode in 3){
      
      
      run_fut(
        ex_model    = modelIN,
        updateModel = FALSE,
        run_version = run_versionIN,
        proj_path  = file.path(run_fl),
        data_path  = file.path(run_fl,"data/in"), 
        model_path = file.path(run_fl,"model"),
        run_path   = file.path(DIR_main,config$model_path),
        multispp   = TRUE,
        rec_num    = rec_mode,
        hcr_num    = hcr_mode,
        plotIT     = TRUE,
        copy2proj  = TRUE,
        est_modIN    = filenm,#assmnt_2023
        flnmIN     = filenm, #assmnt_2023
        ctl_file   = ctl_2,  #asmnt2023_2A
        overwrite  = TRUE,
        debug      = FALSE,
        skip_est   = FALSE,
        configIN   = config,
        DIR_mainIN   = DIR_main)
      
    }
  }
}


# HarvestMode == 3 get X% target of B0 HCR = 1.7atf first (to get B0 for pcod pollock), such that no B B35%
# HarvestMode == 11 use set F
# HarvestMode == 3 , HCR = 1.8  <- set B0 for B40 proxy
#setwd(main)
#source(file.path(DIR_main,"R/sub_fun/ASSESSMENT_RUN_FUN.R"))

hcrset <- 1.8

#______________________________________________
# finally, run model with set values for F (maxABC) to get catch :climate naive
#______________________________________________
  # updated set_catch file:
  #catchh  <- round(maxABC[1,])
  fn      <- file.path(data_fl0,"set_catch.dat")
  cat(c(2,2,2),file=fn,append=FALSE,sep=" ") # set to use set F
  cat("",file=fn,append=TRUE,sep="\n")
  cat(as.numeric(FABC1[1,]),file=fn,append=TRUE,sep=" ")
  cat("",file=fn,append=TRUE,sep="\n")
  cat(12345,file=fn,append=TRUE,sep=" ")
  
  # system(paste0("cd ",DIR_main,
  #               "/src/ceattle-master; ./CEATTLE_run_fut.sh 0 -r 1 -h 11 -f ",
  #               filenm,"_getFabc -m ",modelnm," -ctl ",ctl_flnm,"_0B -plot"))
  # setwd(DIR_main)
  run_fut(
    ex_model    = modelIN,
    updateModel = FALSE,
    run_version = run_versionIN,
    #data_path  = file.path(run_fl,"runs"),
    proj_path  = run_fl,
    data_path  = file.path(run_fl,"data/in"), 
    model_path = file.path(run_fl,"model"),
    run_path   = file.path(DIR_main,config$model_path),
    multispp   = FALSE,
    ctl_file   = paste0(subctl,"_0B"),  #asmnt2023_2A
    rec_num    = 1,
    hcr_num    = 11, # HarvestMode == 11 use set F
    plotIT     = TRUE,
    copy2proj  = TRUE,
    flnmIN     = paste0(modelnm,"_setFABC_CN"), #assmnt_2023
    est_modIN   = modelnm,
    overwrite  = TRUE,
    debug      = FALSE,
    skip_est   = FALSE,
    configIN   = config,
    DIR_mainIN   = DIR_main)
  
  run_fut(
    ex_model    = modelIN,
    updateModel = FALSE,
    run_version = run_versionIN,
    #data_path  = file.path(run_fl,"runs"),
    proj_path  = run_fl,
    data_path  = file.path(run_fl,"data/in"), 
    model_path = file.path(run_fl,"model"),
    run_path   = file.path(DIR_main,config$model_path),
    multispp   = FALSE,
    ctl_file   = paste0(subctl,"_0B"),  #asmnt2023_2A
    rec_num    = 1,
    hcr_num    = 11, # HarvestMode == 11 use set F
    plotIT     = TRUE,
    copy2proj  = TRUE,
    flnmIN     = paste0(modelnm,"_setFABC_CN"), #assmnt_2023
    est_modIN   = modelnm,
    overwrite  = TRUE,
    debug      = FALSE,
    skip_est   = FALSE,
    configIN   = config,
    DIR_mainIN   = DIR_main)
  
  if(1==10){
    msm_mode <-0
    rec_num <-1
    hcr_num <- 11
    PLOT_CEATTLE_FUT(
      multisppIN = FALSE,
      DIR_mainIN = DIR_main,
      proj_pathIN = file.path(run_fl),
      run_versionIN = run_versionIN,
      est_modIN2 = modelnm,
      subflIN    = paste0(paste0(modelnm,"_setFABC_CN"),"_",msm_mode,"_",rec_num,"_",hcr_num),
      flnmIN2    = paste0(modelnm,"_setFABC_CN"),
      run_folderIN =  file.path(file.path(run_fl),"runs",run_versionIN,paste0(modelnm,"_",msm_mode)),
      rec_numIN   = rec_num,
      hcr_numIN   = hcr_num,
      ctl_fileIN  = paste0(subctl,"_0B"))
  }

  # updated set_catch file:
  catchh   <- maxABC[2,]
  fn       <- file.path(data_fl2,"set_catch.dat")
  cat(c(2,2,2),file=fn,append=FALSE,sep=" ");cat("",file=fn,append=TRUE,sep="\n")
  cat(FABC1[2,],file=fn,append=TRUE,sep=" ");cat("",file=fn,append=TRUE,sep="\n")
  cat(12345,file=fn,append=TRUE,sep=" ")

  # system(paste0("cd ",DIR_main,"/src/ceattle-master; ./CEATTLE_run_fut.sh 2 -r 1 -h 11 -f ",
  # 	  filenm,"_getFabc -m ",modelnm," -ctl ",ctl_flnm,"_2B -plot"))
  # 
  run_fut(
    ex_model       = modelIN,
    updateModel = FALSE,
    run_version = run_versionIN,
    proj_path  = file.path(run_fl),
    data_path  = file.path(run_fl,"data/in"), 
    model_path = file.path(run_fl,"model"),
    run_path   = file.path(DIR_main,config$model_path),
    multispp   = TRUE,
    ctl_file   = paste0(subctl,"_2B"), 
    rec_num    = 1,
    hcr_num    = 11, # HarvestMode == 11 use set F
    plotIT     = TRUE,
    copy2proj  = TRUE,
    flnmIN     = paste0(modelnm,"_setFABC_CN"),#"_getFabc", #assmnt_2023
    est_modIN   = modelnm, #asmnt2023_2A
    overwrite  = TRUE,
    debug      = FALSE,
    skip_est   = FALSE,
    configIN   = config,
    DIR_mainIN   = DIR_main)
  #______________________________________________
  # finally, run model with set values for catch (maxABC) to get Fabc: CLIMATE proj
  #______________________________________________
  #//      3 = RS function based on top AIC selected environm. parms for each spp (rs_data4CEATTLE_TOP)
  #//      4 = RS function based on model with top R2 value (rs_data4CEATTLE_TopR2)
  #//      5 = RS function based on Recfile_name (above)  
  
  # updated set_catch file to read in set F:
  catchh  <- round(maxABC[1,])
  fn      <- file.path(data_fl0,"set_catch.dat")
  cat(c(2,2,2),file=fn,append=FALSE,sep=" ");cat("",file=fn,append=TRUE,sep="\n")
  cat(FABC1[1,],file=fn,append=TRUE,sep=" ");cat("",file=fn,append=TRUE,sep="\n")
  cat(12345,file=fn,append=TRUE,sep=" ")

  SSM_report_11<-list()
  rset <- c(2,3)
  rset <- c(3)
for(rec_mode in rset){
    # system(paste0("cd ",DIR_main,"/src/ceattle-master; ./CEATTLE_run_fut.sh 0 -r ",r," -h 11 -f ",
    #   filenm,"_getFabc -m ",modelnm," -ctl ",ctl_flnm,"_0B -plot"))
  
      run_fut(
        ex_model    = modelIN,
        updateModel = FALSE,
        run_version = run_versionIN,
        #data_path  = file.path(run_fl,"runs"),
        proj_path  = run_fl,
        data_path  = file.path(run_fl,"data/in"), 
        model_path = file.path(run_fl,"model"),
        run_path   = file.path(DIR_main,config$model_path),
        multispp   = FALSE,
        ctl_file   = paste0(subctl,"_0B"),  #asmnt2023_2A
        rec_num    = rec_mode,
        hcr_num    = 11,# HarvestMode == 11 use set F
        plotIT     = TRUE,
        copy2proj  = TRUE,
        flnmIN     = paste0(modelnm,"_getFabc"), #assmnt_2023
        est_modIN   = modelnm,
        overwrite  = TRUE,
        debug      = FALSE,
        skip_est   = FALSE,
        configIN   = config,
        DIR_mainIN   = DIR_main)
    
  }

  
  # updated set_catch file:
  catchh   <- maxABC[2,]
  fn       <- file.path(data_fl2,"set_catch.dat")
  cat(c(2,2,2),file=fn,append=FALSE,sep=" ");cat("",file=fn,append=TRUE,sep="\n")
  cat(FABC1[2,],file=fn,append=TRUE,sep=" ");cat("",file=fn,append=TRUE,sep="\n")
  cat(12345,file=fn,append=TRUE,sep=" ")
  
  MSM_report11<-list()
  for(rec_mode in rset){
    #     system(paste0("cd ",DIR_main,"/src/ceattle-master; ./CEATTLE_run_fut.sh 2 -r ",r," -h 11 -f ",
    # 	    filenm,"_getFabc -m ",modelnm," -ctl ",ctl_flnm,"_2B -plot"))
    #    setwd(DIR_main)
    
        run_fut(
          ex_model    = modelIN,
          updateModel = FALSE,
          run_version = run_versionIN,
          #data_path  = file.path(run_fl,"runs"),
          proj_path  = run_fl,
          data_path  = file.path(run_fl,"data/in"), 
          model_path = file.path(run_fl,"model"),
          run_path   = file.path(DIR_main,config$model_path),
          multispp   = TRUE,
          ctl_file   = paste0(subctl,"_2B"),  #asmnt2023_2A
          rec_num    = rec_mode,
          hcr_num    = 11, # HarvestMode == 11 use set F
          plotIT     = TRUE,
          copy2proj  = TRUE,
          flnmIN     = paste0(modelnm,"_getFabc"), #assmnt_2023
          est_modIN   = modelnm,
          overwrite  = TRUE,
          debug      = FALSE,
          skip_est   = FALSE,
          configIN   = config,
          DIR_mainIN   = DIR_main)
       
    
  }
  
# now write to the dat_input_files/setFabcFofl.dat and run with sloping HCR?

#______________________________________________
# STEP 5: copy files over to docs/2020_MSMassmnt/data/runs folder
#______________________________________________

# 
# # first the runs folder
# if(!file.exists(file.path(run_fl,"data")))
#   dir.create(file.path(run_fl,"data"))
# 
# if(file.exists(file.path(run_fl,"data/runs")))
#     file.rename(from=file.path(run_fl,"data/runs"),to=file.path(run_fl,paste0("data/runs_",Sys.time())))
# dir.create(file.path(run_fl,"data/runs"))
# file.copy(from=fl_0,to=file.path(run_fl,"data/runs/"),overwrite=TRUE,recursive=TRUE)
# file.copy(from=fl_2,to=file.path(run_fl,"data/runs/"),overwrite=TRUE,recursive=TRUE)
# 
# # then the data_files the runs folder
# if(file.exists(file.path(run_fl,"data/dat_input_files")))
#   file.rename(
#     from=file.path(run_fl,"data/dat_input_files"),
#     to=file.path(run_fl,paste0("data/dat_input_files_",Sys.time())))
# dir.create(file.path(run_fl,"data/dat_input_files"))
# file.copy(
#     from=data_fl,
#     to=file.path(run_fl,"data/dat_input_files/"),overwrite=TRUE,recursive=TRUE)

# q("no")      	  