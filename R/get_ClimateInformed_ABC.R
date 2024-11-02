#'
#'
#'
#'
#'get_ClimateInformed_ABC
#'
#'
#' HarvestMode == 3 get X% target of B0 HCR = 1.7atf first (to get B0 for pcod pollock), such that no B B35%
#' HarvestMode == 11 use set F
#' HarvestMode == 3 , HCR = 1.8  <- set B0 for B40 proxy
#' recMode : recruitment mode
#'  0 = project under mean  rec   (no RS)
#'  1 = mean RS function (mean RS no covariates, ricker; rs_data4CEATTLE_0_0_0)
#'  2 = RS function plus SST (rs_data4CEATTLE_SST
#'  3 = RS function based on top AIC selected environm. parms for each spp (rs_data4CEATTLE_TOP)
#'  4 = RS function based on model with top R2 value (rs_data4CEATTLE_TopR2)
#'  5 = RS function based on Recfile_name (above)  
#'  6 = RS function plus bottom temp 
#'  7 = RS function plus Cold Pool
#'  8 = RS function plus full model plus EAT (tot)

cat("-------- now geting CI-ABC --------\n ")
rset   <- c(1:7)
hcrset <- 1.7
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
                           ref_yrsIN  =  -4:0,
                           BplusYrs = thisYr + c(1,2))

B0_list[["B0_2"]] <- getB0(mn     = file.path(run_fl,"runs",run_versionIN),
                           flname = filenm, 
                           rec    = r, 
                           hvst   = h, 
                           mode   = 2,
                           hcr    = hcrr,
                           ref_yrsIN  =  -4:0,
                           BplusYrs = thisYr + c(1,2))

#-------------------------------------
# REPLACE c_mult (1,7) with (1,8) and B0_set with B0 from no-climate runs
#-------------------------------------   	
#' HarvestMode == 3 , HCR = 1.8  <- set B0 for B40 proxy
# first for single spp:
# SET CLIMATE NAIVE B0
tt0  <- B0_list[["B0_0"]]
tmp0 <- tt0[tt0$Scen==1,]$targetSSB0 

# copy these values to asmnt2020_0B.ctl
replace_ctl(
  flin=file.path(ctl_main0,paste0(subctl,"_0A.ctl")),
  flout=file.path(ctl_main0,paste0(subctl,"_0B.ctl")),
  nm="c_mult",rplac=c(1,8))

replace_ctl(
  flin=file.path(ctl_main0,paste0(subctl,"_0B.ctl")),
  flout=file.path(ctl_main0,paste0(subctl,"_0B.ctl")),
  nm="B0_set",rplac=tmp0)

# then for multi spp:
tt2  <- B0_list[["B0_2"]]
tmp2 <- tt2[tt2$Scen==1,]$targetSSB0    

# copy these values 1.8 is use set B0
replace_ctl(
  flin=file.path(ctl_main2,paste0(subctl,"_2A.ctl")),
  flout=file.path(ctl_main2,paste0(subctl,"_2B.ctl")),
  nm="c_mult",rplac=c(1,8))

replace_ctl(
  flin=file.path(ctl_main2,paste0(subctl,"_2B.ctl")),
  flout=file.path(ctl_main2,paste0(subctl,"_2B.ctl")),
  nm="B0_set",rplac=tmp2)

replace_ctl(
  flin=file.path(ctl_main2,paste0(subctl,"_2B.ctl")),
  flout=file.path(ctl_main2,paste0(subctl,"_2B.ctl")),
  nm="msmMode",rplac=2)

#______________________________________________	 
# update B0_list and save
#______________________________________________
B0_list[["setB0vals"]]<-data.frame(
  ssm=B0_list[["B0_0"]][B0_list[["B0_0"]]$Scen==1,]$targetSSB0,
  msm=B0_list[["B0_2"]][B0_list[["B0_2"]]$Scen==1,]$targetSSB0)

B0_list[["setBfvals"]]<-data.frame(
  ssm=B0_list[["B0_0"]][B0_list[["B0_0"]]$Scen==1,]$SSB,
  msm=B0_list[["B0_2"]][B0_list[["B0_2"]]$Scen==1,]$SSB)
B0_list[["efctv_B2100ratio"]]<-B0_list[["setBfvals"]]/B0_list[["setB0vals"]]

save(B0_list,file=file.path(fl_0,"B0_list.Rdata"))
save.image(file=file.path(fl_0,"assmntStuffA.Rdata"))


SSM_report <- list()
# Single species
for(rec_mode in rset){

  run_fut(
    ex_model    = modelIN,
    updateModel = FALSE,
    run_version = run_versionIN,
    # proj_path  = file.path(run_fl),
    # data_path  = file.path(run_fl,"data/in"), 
   # model_path = file.path(run_fl,"model"),
    proj_path  = file.path(run_fl),
    data_pathIN  = file.path(run_fl,"data/in"), 
    model_pathIN = DIR_main,
    run_path   = file.path(DIR_main,config$model_path),
    multispp   = FALSE,
    ctl_file   = ctl_0,  #asmnt2022_0A
    rec_num    = rec_mode,
    hcr_num    = 3,
    plotIT     = TRUE,
    copy2proj  = TRUE,
    est_modIN    = modelnm,#assmnt_2022
    flnmIN     = paste0(modelnm,"_getFABC_CI"),
    overwrite  = TRUE,
    debug      = FALSE,
    skip_est   = FALSE,
    configIN   = config,
    DIR_mainIN   = DIR_main)
  # SSM_report[[as.character(rec_mode)]]<-tryCatch(
  #     {
  #      
  #       run_fut(
  #         ex_model    = modelIN,
  #         updateModel = FALSE,
  #         run_version = run_versionIN,
  #         proj_path  = file.path(run_fl),
  #         data_path  = file.path(run_fl,"data/in"), 
  #         model_path = file.path(run_fl,"model"),
  #         run_path   = file.path(DIR_main,config$model_path),
  #         multispp   = FALSE,
  #         ctl_file   = ctl_0,  #asmnt2022_0A
  #         rec_num    = rec_mode,
  #         hcr_num    = 3,
  #         plotIT     = TRUE,
  #         copy2proj  = TRUE,
  #         est_modIN    = modelnm,#assmnt_2022
  #         flnmIN     = paste0(modelnm,"_getFABC_CI"),
  #         overwrite  = TRUE,
  #         debug      = FALSE,
  #         skip_est   = FALSE,
  #         configIN   = config,
  #         DIR_mainIN   = DIR_main)
  #       return(0)
  #     },
  #     error=function(cond) {
  #       message("problem with projection")
  #      
  #       # Choose a return value in case of error
  #       return("error")
  #     },
  #     warning=function(cond) {
  #       message(paste("projection caused a warning:"))
  #     
  #       # Choose a return value in case of warning
  #       return("warning")
  #     },
  #     finally={
  #       
  #     }
  #   )    
    
}

MSM_report<-list()
# Now for multispecies
for(rec_mode in rset){
  run_fut(
    ex_model    = modelIN,
    updateModel = FALSE,
    run_version = run_versionIN,
    # proj_path  = file.path(run_fl),
    # data_path  = file.path(run_fl,"data/in"), 
   # model_path = file.path(run_fl,"model"),
    proj_path  = file.path(run_fl),
    data_pathIN  = file.path(run_fl,"data/in"), 
    model_pathIN = DIR_main,
   
    run_path   = file.path(DIR_main,config$model_path),
    multispp   = TRUE,
    rec_num    = rec_mode,
    hcr_num    = 3,
    plotIT     = TRUE,
    copy2proj  = TRUE,
    est_modIN  = modelnm,#assmnt_2022
    flnmIN     = paste0(modelnm,"_getFABC_CI"),
    ctl_file   = ctl_2,  #asmnt2022_2A
    overwrite  = TRUE,
    debug      = FALSE,
    skip_est   = FALSE,
    configIN   = config,
     DIR_mainIN   = DIR_main)
  # MSM_report[[as.character(rec_mode)]]<-tryCatch(
  #   {
  #     run_fut(
  #       ex_model    = modelIN,
  #       updateModel = FALSE,
  #       run_version = run_versionIN,
  #       proj_path  = file.path(run_fl),
  #       data_path  = file.path(run_fl,"data/in"), 
  #       model_path = file.path(run_fl,"model"),
  #       run_path   = file.path(DIR_main,config$model_path),
  #       multispp   = TRUE,
  #       rec_num    = rec_mode,
  #       hcr_num    = 3,
  #       plotIT     = TRUE,
  #       copy2proj  = TRUE,
  #       est_modIN  = modelnm,#assmnt_2022
  #       flnmIN     = paste0(modelnm,"_getFABC_CI"),
  #       ctl_file   = ctl_2,  #asmnt2022_2A
  #       overwrite  = TRUE,
  #       debug      = FALSE,
  #       skip_est   = FALSE,
  #       configIN   = config,
  #       DIR_mainIN   = DIR_main)
  # 
  #     return(0)
  #   },
  #   error=function(cond) {
  #     message("problem with projection")
  #     
  #     # Choose a return value in case of error
  #     return("error")
  #   },
  #   warning=function(cond) {
  #     message(paste("projection caused a warning:"))
  #     
  #     # Choose a return value in case of warning
  #     return("warning")
  #   },
  #   finally={
  #     
  #   }
  # )    
    

}


#______________________________________________
# Now using ABC as mean catch at 2095-2100, and F40, B40, and B0 from above, project under set C=ABC,
# in order to get F40 in 2018, and F40 in 2019
# Update " dat_input_files/set_catch.dat" files in order to set_val for catch = ABC from step 3
#______________________________________________
#create a data file for each sub scenario
#setwd(fl_0)


# Load projections
load(file.path(fl_0,"projections",paste0(modelnm,"_getFABC_CI","_0_1_3/results/proj.Rdata")))
m_0_1_3  <- proj ; rm(proj) # loads the proj data file
load(file.path(fl_2,"projections",paste0(modelnm,"_getFABC_CI","_2_1_3/results/proj.Rdata")))
m_2_1_3  <- proj ; rm(proj)
proj <-"test"

#______________________________________________
# get maxABC as the last 5 years mean ABC (of projection period)
#______________________________________________
# 4:6  is the fishing scenarios for plk, pcod, and atf, 1:3 is F=0
maxABC_0  <-  apply(rev(m_0_1_3$ts$catch[	m_0_1_3$ts$meta$fut_simulation==1,][4:6,])[,1:5],1,mean)
maxABC_2  <-  apply(rev(m_2_1_3$ts$catch[	m_2_1_3$ts$meta$fut_simulation==1,][4:6,])[,1:5],1,mean)
maxABC    <-  rbind(maxABC_0,maxABC_2)
#rbind(maxABC_0,maxABC_2)

#______________________________________________
# get next year and following year vals
#______________________________________________

parm0_yr1  <- getFparms(mode=0,scen=1,mn=file.path(run_fl,"runs",run_versionIN),flname=paste0(modelnm,"_getFABC_CI"),
                        modelname=modelnm,rec =1, hvst=3,hcr=hcrset,yr=1+nyrs,endyr=nyrs,catch=maxABC_0)
parm2_yr1  <- getFparms(mode=2,scen=1,mn=file.path(run_fl,"runs",run_versionIN),flname=paste0(modelnm,"_getFABC_CI"),
                        modelname=modelnm,rec =1, hvst=3,hcr=hcrset,yr=1+nyrs,endyr=nyrs,catch=maxABC_0)

parm0_yr2  <- getFparms(mode=0,scen=1,mn=file.path(run_fl,"runs",run_versionIN),flname=paste0(modelnm,"_getFABC_CI"),
                        modelname=modelnm,rec =1, hvst=3,hcr=hcrset,yr=2+nyrs,endyr=nyrs,catch=maxABC_0)
parm2_yr2  <- getFparms(mode=2,scen=1,mn=file.path(run_fl,"runs",run_versionIN),flname=paste0(modelnm,"_getFABC_CI"),
                        modelname=modelnm,rec =1, hvst=3,hcr=hcrset,yr=2+nyrs,endyr=nyrs,catch=maxABC_0)


#______________________________________________
# Get the FABC corresponding to those vals of ABC (yr+1 and yr+2)
#______________________________________________
ss     <- 1
findF(par=log(.8),data=list(targetC=maxABC[1,ss],parmIn=parm0_yr1,spIn=ss))
getC(.8,data=list(parmIn=parm0_yr1,spIn=ss))

FABC   <- maxABC*0
for(ss in 1:3){
  FABC[1,ss] <- exp(optimize(findF,
                             interval = log(c(0.0001,4)),
                             data     = list(
                               targetC = maxABC[1,ss],
                               parmIn  = parm0_yr1,
                               spIn    = ss))$minimum)
  FABC[2,ss] <- exp(optimize(findF,
                             interval = log(c(0.0001,4)),
                             data     = list(
                               targetC = maxABC[2,ss],
                               parmIn  = parm2_yr1,
                               spIn    = ss))$minimum)
}
FABC1  <- FABC

FABC   <- maxABC*0
for(ss in 1:3){
  FABC[1,ss]<-exp(optimize(findF,interval=log(c(0.0001,
                                                4)),data=list(targetC=maxABC[1,ss],parmIn=parm0_yr2,spIn=ss))$minimum)
  FABC[2,ss]<-exp(optimize(findF,interval=log(c(0.0001,
                                                4)),data=list(targetC=maxABC[2,ss],parmIn=parm2_yr2,spIn=ss))$minimum)
}
FABC2  <- FABC
rm(FABC)

# Save everything
save(maxABC,file=file.path(file.path(run_fl,"runs",run_versionIN),"maxABC.Rdata"))
save(FABC1,file=file.path(file.path(run_fl,"runs",run_versionIN), "FABC1.Rdata"))
save(FABC2,file=file.path(file.path(run_fl,"runs",run_versionIN), "FABC2.Rdata"))
save.image(file=file.path(file.path(run_fl,"runs",run_versionIN), "assmntStuff.Rdata"))

cat("----- get CI ABC part 1 complete -------------")
