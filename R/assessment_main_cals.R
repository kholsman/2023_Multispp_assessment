#'assessment_main_cals.R
#'
#'@author Kirstin Holsman
#'
#'@description
#'
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
#'  
# rm(list=ls());setwd("/Volumes/LaCie/GitHub_cloud/CEATTLE_projects/2023_Multispp_assessment/docs/MSM_Assessment")
thisYr <- as.numeric(format(Sys.time(), "%Y"))
today  <- format(Sys.time(), "%b %d, %Y")
print(getwd())
suppressWarnings(source("../Assessment_setup.R"))
figfile    <- file.path(out_main,"R_figures")
load(file.path(figfile,"plotdata.Rdata"))
suppressWarnings(source("../Assessment_setup.R")) # again to fix paths
#rm(tmp)
figs_fldr<-"../MSM_Assessment/Figs" 

run_versionIN
tmpfl     <- file.path(run_fl,"runs",run_versionIN,paste0(filenm,"_2"))

datfluse  <- file.path(tmpfl,dir(tmpfl)[rev(grep("dat_input_files_",dir(tmpfl)))[1]])
datfluse  <- file.path(run_fl,"data/in/dat_input_files")
ctluse    <- dir(tmpfl)[rev(grep("Control_files_",dir(tmpfl)))[1]]
ctldat    <- read_ctl(file.path(ctl_main2,paste0(ctl_2,".ctl")))
#ctldat    <- read_ctl(file.path(fl_2,ctluse,paste0(ctl_2,".ctl")))
futdatfl  <- rev(unlist(strsplit(ctldat$futfile_name,"/")))[1]
gcmscen    <- get_Modelnames(file.path(datfluse,futdatfl))

ifile <- scan(file.path(datfluse,futdatfl), what="character",flush=T,blank.lines.skip=F, quiet=T)
models <- scan(file.path(datfluse,futdatfl),nlines =1,skip = grep("Models",ifile)[1], 
               what="character",flush=F,blank.lines.skip=T, quiet=T,sep="\n")
models <-  strsplit(models,"# ")[[1]][-1]
models <- gsub(" ","",models)

gcmscen      <- gsub("MIROC","miroc",gcmscen)
gcmscen      <- gsub("CESM","cesm",gcmscen)
gcmscen      <- gsub("GFDL","gfdl",gcmscen)
scens     <- c("mnhind",unlist(lapply(strsplit(gcmscen[-1],"_"),"[[",2)))
gcms      <- unlist(lapply(strsplit(gcmscen,"_"),"[[",1))

# now set up the docs folder
EcoCons_path <- file.path(run_fl,"docs/EcoCons")
assmnt_path  <- file.path(run_fl,"docs/MSM_Assessment") 

if(!dir.exists(file.path(run_fl,"docs")))
  dir.create(file.path(run_fl,"docs"))
if(!dir.exists(EcoCons_path))
  dir.create(EcoCons_path)
if(!dir.exists(assmnt_path))
  dir.create(assmnt_path)

blues      <- RColorBrewer::brewer.pal(5, "Blues")
BG         <- RColorBrewer::brewer.pal(9, "GnBu") 
oranges    <- RColorBrewer::brewer.pal(5, "Oranges")

#---------------------------------------------------

load(file.path(fl_0,"B0_list.Rdata")) # loads B0_list
load(file=file.path(out_main,"maxABC.Rdata"))
load(file=file.path(out_main,"FABC1.Rdata"))
load(file=file.path(out_main,"FABC2.Rdata"))
load(file.path(run_fl,"data/in/dietdata/diet_p4CEATTLE.Rdata"))


if(1==10){
  
  plot_M2(coll=c(colors()[330],"black"),ltyy=c(2,1))
  quartz.save(file="Fig1.jpg",type="jpg",dpi=500, width=4,height=6)
  plot_propM()
  quartz.save(file="Fig2.jpg",type="jpg",dpi=500, width=6,height=4)
  plot_eaten3(units=1e9)
  quartz.save(file="Fig3.jpg",type="jpg",dpi=500, width=4,height=6)
}

# change in SSB in the model from this and last year (not between assessment years)
deltaSSB  <- matrix(0,nspp,3)
for(s in 1:nspp){
  SSBtmp         <- eval(parse(text=paste0("ceattle_2$BiomassSSB_",s)))[1,]
  tgt            <- SSBtmp[nyrs-1]
  deltaSSB[s,1]  <- ((SSBtmp-tgt)/tgt)[nyrs]
  tgt            <- SSBtmp[nyrs-2]
  deltaSSB[s,2]  <- ((SSBtmp-tgt)/tgt)[nyrs]
  tgt            <- mean(SSBtmp[1:37]) # 1979-2015
  deltaSSB[s,3]  <- ((SSBtmp-tgt)/tgt)[nyrs]
}

colnames(deltaSSB) <- c("vsLastyr","vs2yrsAgo","vsMean")
deltaB             <- matrix(0,nspp,4)

for(s in 1:nspp){
  Btmp            <- eval(parse(text=paste0("ceattle_2$Biomass_",s)))[1,]
  tgt             <- Btmp[nyrs-1]
  deltaB[s,1]     <- ((Btmp-tgt)/tgt)[nyrs]
  tgt             <- Btmp[nyrs-2]
  deltaB[s,2]     <- ((Btmp-tgt)/tgt)[nyrs]
  tgt             <- Btmp[which(years==2016)]
  deltaB[s,3]     <- ((Btmp-tgt)/tgt)[nyrs]
  tgt             <- mean(Btmp[1:37]) # 1979-2015
  deltaB[s,4]     <- ((Btmp-tgt)/tgt)[nyrs]
}

colnames(deltaB) <- c("vsLastyr","vs2yrsAgo","vs2016","vsMean")
deltaR           <- matrix(0,nspp,4)

for(s in 1:nspp){
  Rtmp            <- eval(parse(text=paste0("ceattle_2$R_",s)))[1,]
  Rtmp            <- as.numeric(exp(ceattle_2$rec_dev[s,]+ceattle_2$ln_mn_rec[s]))
  tgt             <- as.numeric(Rtmp[nyrs-1])
  deltaR[s,1]     <- ((Rtmp-tgt)/tgt)[nyrs]
  tgt             <- as.numeric(Rtmp[nyrs-2])
  deltaR[s,2]     <- ((Rtmp-tgt)/tgt)[nyrs]
  tgt             <- as.numeric(Rtmp[which(years==2016)])
  deltaR[s,3]     <- ((Rtmp-tgt)/tgt)[nyrs]
  tgt             <- as.numeric(mean(Rtmp[1:37])) # 1979-2015
  deltaR[s,4]     <- ((Rtmp-tgt)/tgt)[nyrs]
}

colnames(deltaR) <- c("vsLastyr","vs2yrsAgo","vs2016","vsMean")

tgt   <- ceattle_2$BiomassSSB_2[1,nyrs-1]; round(100*(ceattle_2$BiomassSSB_2[1,]-tgt)/tgt)[nyrs]
s     <- 1
tt1   <- c(
  mean      = mean(ceattle_2$M2_1[,s]),
  sd        = sd(ceattle_2$M2_1[,s]),
  lower     = mean(ceattle_2$M2_1[,s])-1.95*(sd(ceattle_2$M2_1[,s])),
  upper     = mean(ceattle_2$M2_1[,s])+1.95*(sd(ceattle_2$M2_1[,s])),
  se        = sd(ceattle_2$M2_1[,s])/sqrt(length(ceattle_2$M2_1[,s])),
  se.lower  = mean(ceattle_2$M2_1[,s])-1.95*(sd(ceattle_2$M2_1[,s])/sqrt(length(ceattle_2$M2_1[,s]))),
  se.upper  = mean(ceattle_2$M2_1[,s])+1.95*(sd(ceattle_2$M2_1[,s])/sqrt(length(ceattle_2$M2_1[,s]))))

s     <- 1
tt2   <- c(
  mean     = mean(ceattle_2$M2_2[,s]),
  sd       = sd(ceattle_2$M2_2[,s]),
  lower    = mean(ceattle_2$M2_2[,s])-1.95*(sd(ceattle_2$M2_2[,s])),
  upper    = mean(ceattle_2$M2_2[,s])+1.95*(sd(ceattle_2$M2_2[,s])),
  se       = sd(ceattle_2$M2_2[,s])/sqrt(length(ceattle_2$M2_2[,s])),
  se.lower = mean(ceattle_2$M2_2[,s])-1.95*(sd(ceattle_2$M2_2[,s])/sqrt(length(ceattle_2$M2_2[,s]))),
  se.upper = mean(ceattle_2$M2_2[,s])+1.95*(sd(ceattle_2$M2_2[,s])/sqrt(length(ceattle_2$M2_2[,s]))))

s     <- 1
tt3   <- c(
  mean     = mean(ceattle_2$M2_3[,s]),
  sd       = sd(ceattle_2$M2_3[,s]),
  lower    = mean(ceattle_2$M2_3[,s])-1.95*(sd(ceattle_2$M2_3[,s])),
  upper    = mean(ceattle_2$M2_3[,s])+1.95*(sd(ceattle_2$M2_3[,s])),
  se       = sd(ceattle_2$M2_3[,s])/sqrt(length(ceattle_2$M2_3[,s])),
  se.lower = mean(ceattle_2$M2_3[,s])-1.95*(sd(ceattle_2$M2_3[,s])/sqrt(length(ceattle_2$M2_3[,s]))),
  se.upper = mean(ceattle_2$M2_3[,s])+1.95*(sd(ceattle_2$M2_3[,s])/sqrt(length(ceattle_2$M2_3[,s]))))

table1  <- data.frame(rbind(plk=tt1,pcod=tt2,atf=tt3))  # table of mean mortality
#tmpp<-plot_propM()
allM2   <- (M2_est_1[[1]][,1])+(M2_est_1[[2]][,1])+(M2_est_1[[3]][,1])
mnpropM <- matrix(0,3,3)
mnpropM[1,1] <- mean((M2_est_1[[1]][,1])/allM2)
mnpropM[1,2] <- mean((M2_est_1[[2]][,1])/allM2)
mnpropM[1,3] <- mean((M2_est_1[[3]][,1])/allM2)
tmpp         <- mnpropM

tot_1   <- (ceattle_2$M2_1[,1]+ceattle_2$M1_1[1,1])
tot_2   <- (ceattle_2$M2_2[,1]+ceattle_2$M1_2[1,1])
tot_3   <- (ceattle_2$M2_3[,1]+ceattle_2$M1_3[1,1])
maxyr_n <- c(
  which(tot_1==max(tot_1)),
  which(tot_2==max(tot_2)),
  which(tot_3==max(tot_3)))

prcnt_change <- 100*round((tot_1[nyrs:(maxyr_n[1]+1)] -tot_1[maxyr_n[1]])/tot_1[maxyr_n[1]],2)

#	`r round(mean((E_est_1[[1]][,1])/((E_est_1[[1]][,1])+(E_est_1[[2]][,1])+(E_est_1[[3]][,1])))*100)`

nyrs<-length(ceattle_0$M2_1[,1])  			
mort_table<-list()
#;colnames(mort_table)<-c("SSM","MSM");rownames(mort_table)<-c("plk","pcod","atf")
mort_table[[1]]      <- c(rev(ceattle_0$M2_1[,1]+ceattle_0$M1_1[1,1])[1],
                          rev(ceattle_2$M2_1[,1]+ceattle_2$M1_1[1,1])[1])
mort_table[[2]]      <- c(rev(ceattle_0$M2_2[,1]+ceattle_0$M1_2[1,1])[1],
                          rev(ceattle_2$M2_2[,1]+ceattle_2$M1_2[1,1])[1])
mort_table[[3]]      <- c(rev(ceattle_0$M2_3[,1]+ceattle_0$M1_3[1,1])[1],
                          rev(ceattle_2$M2_3[,1]+ceattle_2$M1_3[1,1])[1])
mort_table_3pls      <- list()

mort_table_3pls[[1]] <- c(mean(apply(ceattle_0$M2_1[,-(1:2)],2,mean)+ceattle_0$M1_1[1,-(1:2)]),
                          mean(apply(ceattle_2$M2_1[,-(1:2)],2,mean)+ceattle_2$M1_1[1,-(1:2)]))
mort_table_3pls[[2]] <- c(mean(apply(ceattle_0$M2_2[,-(1:2)],2,mean)+ceattle_0$M1_2[1,-(1:2)]),
                          mean(apply(ceattle_2$M2_2[,-(1:2)],2,mean)+ceattle_2$M1_2[1,-(1:2)]))
mort_table_3pls[[3]] <- c(mean(apply(ceattle_0$M2_3[,-(1:2)],2,mean)+ceattle_0$M1_3[1,-(1:2)]),
                          mean(apply(ceattle_2$M2_3[,-(1:2)],2,mean)+ceattle_2$M1_3[1,-(1:2)]))

#|2016 total (age 3+) biomass (t)       | 1.46468e+07| 15043940.000| 1313105.000| 1308296.000| 517976.000| 513575.000|
Bio_3pls    <- list()
# Bio_3pls[[1]]<-c(sum(ceattle_0$biomassByage_1[nyrs,-(1:2)]),
#                  sum(ceattle_2$biomassByage_1[nyrs,-(1:2)]))
# Bio_3pls[[2]]<-c(sum(ceattle_0$biomassByage_2[nyrs,-(1:2)]),
#                  sum(ceattle_2$biomassByage_2[nyrs,-(1:2)]))
# Bio_3pls[[3]]<-c(sum(ceattle_0$biomassByage_3[nyrs,-(1:2)]),
#                  sum(ceattle_2$biomassByage_3[nyrs,-(1:2)]))
# nages<-c(ceattle_0$nages_1,ceattle_0$nages_2,ceattle_0$nages_3)

Bio_3pls[[1]] <- c(sum( (ceattle_0$N_1[nyrs+1,]*  ceattle_0$wt_1[nyrs,][1:nages[1]])[-(1:2)] ),
                   sum( (ceattle_2$N_1[nyrs+1,]*  ceattle_2$wt_1[nyrs,][1:nages[1]])[-(1:2)] ))
Bio_3pls[[2]] <- c(sum( (ceattle_0$N_2[nyrs+1,]*  ceattle_0$wt_2[nyrs,][1:nages[2]])[-(1:2)] ),
                   sum( (ceattle_2$N_2[nyrs+1,]*  ceattle_2$wt_2[nyrs,][1:nages[2]])[-(1:2)] ))
Bio_3pls[[3]] <- c(sum( (ceattle_0$N_3[nyrs+1,]*  ceattle_0$wt_3[nyrs,][1:nages[3]])[-(1:2)] ),
                   sum( (ceattle_2$N_3[nyrs+1,]*  ceattle_2$wt_3[nyrs,][1:nages[3]])[-(1:2)] ))
#|2016 SSB (female spawning biomass; t) | 5.41804e+06|  5570280.000|  241631.000|  239867.000| 375576.000| 372533.000|

# which projection set to use for the ensemble calculations?
# m0futTXT <- "0_1_11"  # fut effects on temp growth and consumption but mean rec
# m2futTXT <- "2_1_11"
# m0futTXT <- "0_3_11"  # fut effects on temp growth and consumption and top AIC rec
# m2futTXT <- "2_3_11"  # fut effects on temp growth and consumption and top AIC rec
# m0futTXT <- "0_4_11"  # fut effects on temp growth and consumption and top R2 rec
# m2futTXT <- "2_4_11"  # fut effects on temp growth and consumption and top R2 rec  

m0futTXT <- "0_5_11"  # fut effects on temp growth and consumption and top Ricker R2 rec
m2futTXT <- "2_5_11"  # fut effects on temp growth and consumption and top Ricker R2 rec  
#6 = BT didn't work why?
# //recMode : recruitment mode                                                 //
# //      0 = project under mean  rec   (no RS)
# //      1 = mean RS function (mean RS no covariates, ricker; rs_data4CEATTLE_0_0_0)
# //      2 = RS function plus SST (rs_data4CEATTLE_SST
# //      3 = RS function based on top AIC selected environm.
# //      4 = RS function based on model with top R2 value (rs_data4CEATTLE_TopR2)
# //      5 = RS function based on Recfile_name (above)   
# [1] "srvy_rep_temp_bottom5m","fall_Ztot_large_integrated","spring_Ztot_large_surface5m""srvy_rep_temp_surface5m"   
# //      6 = RS function plus bottom temp 
# //      7 = RS function plus Cold Pool
load(file.path(fl_0,"projections",paste0(modelnm,"_0_1_3/results/proj.Rdata"))) 
m_0_1_3  <- proj ; rm(proj) # loads the proj data file
load(file.path(fl_2,"projections",paste0(modelnm,"_2_1_3/results/proj.Rdata")))
m_2_1_3  <- proj ; rm(proj)

load(file.path(fl_0,"projections",paste0(modelnm,paste0("_getFabc_",m0futTXT,"/results/proj.Rdata"))))
m_0_FUT <- proj ; rm(proj) #m0futTXT <- "0_1_11"
load(file.path(fl_2,"projections",paste0(modelnm,paste0("_getFabc_",m2futTXT,"/results/proj.Rdata"))))
m_2_FUT <- proj ; rm(proj)

SSBlastyr<-SSBnow<-SSB<-SSB_2<-list()


cc0  <- which(m_0_FUT$ts$meta$fut_simulation==1&m_0_FUT$ts$catch$'2030'!=0)
cc2  <- which(m_2_FUT$ts$meta$fut_simulation==1&m_2_FUT$ts$catch$'2030'!=0)
# cc1  <- which(m_0_FUT$ts$meta$fut_simulation==1&m_0_FUT$ts$catch$'2030'!=0)
# cc12 <- which(m_2_FUT$ts$meta$fut_simulation==1&m_2_FUT$ts$catch$'2030'!=0)
# cc0  <- which(m_0_1_3$ts$meta$fut_simulation==1&m_0_1_3$ts$meta$B_target!=0)
# cc2  <- which(m_2_1_3$ts$meta$fut_simulation==1&m_2_1_3$ts$meta$B_target!=0)
# cc1  <- which(m_0_1_3$ts$meta$fut_simulation==1&m_0_1_3$ts$meta$B_target==0)
# cc12 <- which(m_2_1_3$ts$meta$fut_simulation==1&m_2_1_3$ts$meta$B_target==0)
yy   <- which(m_0_1_3$ts$years==thisYr+1)

for(s in 1:3){
  # fished SSB for scenario 1 starts at 3
  ## SSB is measured at the start of the year so 2019 is the SSB AT the START of 2019
  SSBlastyr[[s]] <- c(
    m_0_FUT$ts$SSB[which(m_0_FUT$ts$years==thisYr-1)][cc0[s],],
    m_2_FUT$ts$SSB[which(m_2_FUT$ts$years==thisYr-1)][cc2[s],])
  
  SSBnow[[s]]    <- c(
    m_0_FUT$ts$SSB[which(m_0_FUT$ts$years==thisYr)][cc0[s],],
    m_2_FUT$ts$SSB[which(m_2_FUT$ts$years==thisYr)][cc2[s],])
  
  SSB[[s]]       <- c(
    m_0_FUT$ts$SSB[which(m_0_FUT$ts$years==thisYr+1)][cc0[s],],
    m_2_FUT$ts$SSB[which(m_2_FUT$ts$years==thisYr+1)][cc2[s],])
  
  SSB_2[[s]]     <- c(
    m_0_FUT$ts$SSB[which(m_0_FUT$ts$years==thisYr+2)][cc0[s],],
    m_2_FUT$ts$SSB[which(m_2_FUT$ts$years==thisYr+2)][cc2[s],])
}

get_val <- function(kind = "SSB",m0 = m_0_1_3,m2 = m_2_1_3,spnames =splist[1:3]){
  #"meta"  "years" "SSB"   "rec"   "catch" "Frate"
  cnames <- names(m0$ts$meta)
  
  m0$ts$meta$B_target[m0$ts$catch$'2030'!=0]<-.4
  m2$ts$meta$B_target[m2$ts$catch$'2030'!=0]<-.4
  
  modes  <- c("Single-species", "","Multispecies")
  tt_cn0 <- reshape2::melt(cbind(m0$ts$meta,m0$ts[[kind]]),
                           id.var=cnames,
                           variable.name = "year",value.name = kind)%>%
    mutate(type = "climate-naive",
           type2 = "CN")%>%filter(fut_simulation ==1)
  tt_cn2 <- reshape2::melt(cbind(m2$ts$meta,m2$ts[[kind]]),
                           id.var=cnames,
                           variable.name = "year",value.name = kind)%>%
    mutate(type = "climate-naive",
           type2 = "CN")%>%filter(fut_simulation ==1)
  
  tt_ci0 <- reshape2::melt(cbind(m0$ts$meta,m_0_FUT$ts[[kind]]),
                           id.var=cnames,
                           variable.name = "year",value.name = kind)%>%
    mutate(type = "climate-integrated",
           type2 = "CI")
  tt_ci2 <- reshape2::melt(cbind(m2$ts$meta,m_2_FUT$ts[[kind]]),
                           id.var=cnames,
                           variable.name = "year",value.name = kind)%>%
    mutate(type = "climate-integrated",
           type2 = "CI")
  out<- rbind(tt_cn0,tt_cn2,tt_ci0,tt_ci2)%>%
    mutate(year = as.numeric(as.character(year)),spnum=species)
  out$species <- factor(spnames[out$spnum],levels=spnames)
  out$type  <- factor(out$type)
  
  out$mode <- factor(modes[out$MSMmode+1], levels=modes)
  out$type_sim <- paste0(out$type,"_",out$fut_simulation)
  out$fished                  <- "fished"
  out$fished[out$B_target==0] <- "unfished"
  out$fished <- factor(out$fished,levels = c("unfished","fished"))
  
  out$scen     <- scens[out$fut_simulation]
  out$gcm      <- gcms[out$fut_simulation]
  out$gcmscen  <- gcmscen[out$fut_simulation]
  out$type_sim2 <- paste0(out$type2,"-",out$gcmscen)
  cmips <- rep("CMIP6",length(scens))
  cmips[grep("rcp",scens)] <- "CMIP5" 
  out$cmip     <- cmips[out$fut_simulation]
  
  
  return(out)
}

SSB_dat   <- get_val("SSB",m0 = m_0_1_3,m2 = m_2_1_3)
catch_dat <- get_val("catch",m0 = m_0_1_3,m2 = m_2_1_3)
Frate_dat <- get_val("Frate",m0 = m_0_1_3,m2 = m_2_1_3)
rec_dat   <- get_val("rec",m0 = m_0_1_3,m2 = m_2_1_3)

pp_SSB <- ggplot()+
  geom_line(data=SSB_dat%>%filter( MSMmode==0,cmip=="CMIP6"),
            aes(x=year,y = SSB/1e6, color=type_sim,linetype=fished),size=.8)+
  facet_grid(species~scen,scales="free_y")+
  scale_color_viridis_d(begin=.8,end=.1,option="mako" )+theme_minimal()+
  scale_linetype_discrete( labels = c("F = 0","F = F_target"))


pal <- c(
  viridis::viridis(n=3, begin = .3,end=.8,direction=1),
  rev(viridis::viridis(n=3, begin = .6,end=.9,direction=1,option = "B")),
  viridis::viridis(n=3, begin = 0,end=.8,direction=1)[1],
  viridis::viridis(n=3, begin = 0,end=.8,direction=1)[1])
pal<-pal[c(1,5,3,4,2,6,7,8)]

ssb_plot0 <- plot_output(datIN = SSB_dat%>%filter(type_sim2!="CI-mnhind"),
                         var = "SSB",
                         modeIN = 0,
                         divby =1e6,
                         cmipIN = "CMIP6",
                         alphaIN = c(0.8,1),
                         sizeIN =c(0.7,1),
                         lineIN = c("dashed","solid"),
                         ylabIN = "Spawning biomass (million t)\n")
ssb_plot2 <- plot_output(datIN = SSB_dat%>%filter(type_sim2!="CI-mnhind"),
                         var = "SSB",
                         modeIN = 2,
                         divby =1e6,
                         cmipIN = "CMIP6",
                         alphaIN = c(0.8,1),
                         sizeIN =c(0.7,1),
                         lineIN = c("dashed","solid"),
                         ylabIN = "Spawning biomass (million t)\n")

abc_plot0 <- plot_output(datIN = catch_dat%>%
                           filter(fished=="fished",type_sim2!="CN-mnhind"),
                         var = "catch",
                         modeIN  = 0,
                         divby   = 1e6,
                         cmipIN  = "CMIP6",
                         alphaIN = c(1,1),
                         sizeIN  = c(1,1),
                         lineIN  = c("solid","solid"),
                         ylabIN  = "ABC (million t)\n")
abc_plot2 <- plot_output(datIN = catch_dat%>%
                           filter(fished=="fished",type_sim2!="CN-mnhind"),
                         var = "catch",
                         modeIN  = 2,
                         divby   = 1e6,
                         cmipIN  = "CMIP6",
                         alphaIN = c(1,1),
                         sizeIN  = c(1,1),
                         lineIN  = c("solid","solid"),
                         ylabIN  = "ABC (million t)\n")

Frate_plot0 <- plot_output(datIN = Frate_dat%>%
                             filter(fished=="fished",type_sim2!="CN-mnhind"),
                           var = "Frate",
                           modeIN  = 0,
                           divby   = 1,
                           cmipIN  = "CMIP6",
                           alphaIN = c(1,1),
                           sizeIN  = c(1,1),
                           lineIN  = c("solid","solid"),
                           ylabIN  = "F_target\n")
Frate_plot2 <- plot_output(datIN = Frate_dat%>%
                             filter(fished=="fished",type_sim2!="CN-mnhind"),
                           var = "Frate",
                           modeIN  = 2,
                           divby   = 1,
                           cmipIN  = "CMIP6",
                           alphaIN = c(1,1),
                           sizeIN  = c(1,1),
                           lineIN  = c("solid","solid"),
                           ylabIN  = "F_target\n")


if(!file.exists("Figs")) dir.create("Figs")
png(file.path(figs_fldr,"Frate_plot0.png"),units="in",width=10,height=6,res=250)
print(Frate_plot0)
dev.off()

png(file.path(figs_fldr,"Frate_plot2.png"),units="in",width=10,height=6,res=250)
print(Frate_plot2)
dev.off()

png(file.path(figs_fldr,"abc_plot0.png"),units="in",width=10,height=6,res=250)
print(abc_plot0)
dev.off()

png(file=file.path(figs_fldr,"ssb_plot0.png"),units="in",width=10,height=6,res=250)
print(ssb_plot0)
dev.off()

png(file.path(figs_fldr,"abc_plot2.png"),units="in",width=10,height=6,res=250)
print(abc_plot2)
dev.off()

png(file=file.path(figs_fldr,"ssb_plot2.png"),units="in",width=10,height=6,res=250)
print(ssb_plot2)
dev.off()

png(file.path(figs_fldr,"ssb_plot_zoom2.png"),units="in",width=10,height=6,res=250)
print(ssb_plot2+
        coord_cartesian(xlim=c(1975,(thisYr+10)))
)
dev.off()

png(file.path(figs_fldr,"ssb_plot_zoom0.png"),units="in",width=10,height=6,res=250)
print(ssb_plot0+
        coord_cartesian(xlim=c(1975,(thisYr+10)))
)
dev.off()



get_SSBESM <- function(s =1, 
                       lab = "lastyr",
                       simlab = "all",
                       mult=1,
                       FUT_set0 = m_0_1_3,
                       FUT_set2 = m_2_1_3,
                       FUT0 = m_0_FUT,
                       FUT2 = m_2_FUT,
                       yrIN = thisYr-1,
                       sims = 1:length(gcmscen)){
  
  FUT_set0$ts$meta$B_target[FUT_set0$ts$catch$'2030'!=0]<-.4
  FUT_set2$ts$meta$B_target[FUT_set2$ts$catch$'2030'!=0]<-.4
  
  
  tt0 <- data.frame(FUT_set0$ts$meta,FUT0$ts$SSB)
  colnames(tt0) <- c(colnames(FUT_set0$ts$meta),colnames(FUT0$ts$SSB))
  
  tt2 <- data.frame(FUT_set2$ts$meta,FUT2$ts$SSB)
  colnames(tt2) <- c(colnames(FUT_set2$ts$meta),colnames(FUT2$ts$SSB))
  
  tmp0 <- reshape2::melt(tt0,id.var=colnames(FUT_set0$ts$meta),
                         variable.name = "year",value.name = "SSB")
  tmp2 <- reshape2::melt(tt2,id.var=colnames(FUT_set2$ts$meta),
                         variable.name = "year",value.name = "SSB")
  out_persist <- rbind(tmp0,tmp2)%>%
    filter(year%in%yrIN,
           B_target!=0,
           species%in%s,
           fut_simulation==1)%>%
    group_by(MSMmode,species)%>%
    summarize(mn   = mean(SSB,na.rm=T),
              sd   = sd(SSB,na.rm=T),
              nobs = length(SSB),
              year = round(mean( as.numeric(as.character(year)),na.rm=T) )) 
  out_today <- rbind(tmp0,tmp2)%>%
    filter(year%in%thisYr,
           B_target!=0,
           species%in%s,
           fut_simulation==1)%>%
    group_by(MSMmode,species)%>%
    summarize(mn   = mean(SSB,na.rm=T),
              sd   = sd(SSB,na.rm=T),
              nobs = length(SSB),
              year = round(mean( as.numeric(as.character(year)),na.rm=T) )) 
  
  out <- rbind(tmp0,tmp2)%>%
    filter(year%in%yrIN,
           B_target!=0,
           species%in%s,
           fut_simulation%in%sims)%>%
    group_by(MSMmode,species)%>%
    summarize(mn   = mean(SSB,na.rm=T),
              sd   = sd(SSB,na.rm=T),
              nobs = length(SSB),
              year = round(mean( as.numeric(as.character(year)),na.rm=T) )) %>%
    mutate(se = sd/sqrt(nobs),
           upper   = mn+(mult*se),
           lower   = mn-(mult*se),
           yearstart = yrIN[1],
           yearend   = rev(yrIN)[1],
           lab    = lab,
           simlab = simlab)%>%ungroup()
  
  out$mn_deltaP    <- (out$mn-out_persist$mn)/out_persist$mn
  out$upper_deltaP <- (out$upper-out_persist$mn)/out_persist$mn
  out$lower_deltaP <- (out$lower-out_persist$mn)/out_persist$mn
  
  out$mn_deltanow    <- (out$mn-out_today$mn)/out_today$mn
  out$upper_deltanow <- (out$upper-out_today$mn)/out_today$mn
  out$lower_deltanow <- (out$lower-out_today$mn)/out_today$mn
  
  return(out)
}


s<-1:3
# SSB at the start of  this yr + 1
dlt_SSB <- suppressMessages(get_SSBESM(s = s, lab = "lastyr",yrIN = thisYr-1 ))

dlt_SSB <- suppressMessages(rbind(dlt_SSB, get_SSBESM(s = s, lab = "now",yrIN = thisYr )))

simsIN  <- 1:length(gcmscen); simlabIN <- "all"
dlt_SSB <- suppressMessages(rbind(dlt_SSB, get_SSBESM(s = s, lab = "yrpls0_all",yrIN = thisYr,
                                                      sims=simsIN,simlab=simlabIN)))
dlt_SSB <- suppressMessages(rbind(dlt_SSB, get_SSBESM(s = s, lab = "yrpls1_all",yrIN = thisYr+1,
                                                      sims=simsIN,simlab=simlabIN)))
dlt_SSB <- suppressMessages(rbind(dlt_SSB, get_SSBESM(s = s, lab = "yrpls2_all",yrIN = thisYr+2,
                                                      sims=simsIN,simlab=simlabIN)))
dlt_SSB <- suppressMessages(rbind(dlt_SSB, get_SSBESM(s = s, lab = "yrpls10_all",yrIN = thisYr+9:11,
                                                      sims=simsIN,simlab=simlabIN)))
dlt_SSB <- suppressMessages(rbind(dlt_SSB, get_SSBESM(s = s, lab = "yr2050_all",yrIN = 2040:2060,
                                                      sims=simsIN,simlab=simlabIN)))
dlt_SSB <- suppressMessages(rbind(dlt_SSB, get_SSBESM(s = s, lab = "yr2080_all",yrIN = 2080:2100,
                                                      sims=simsIN,simlab=simlabIN)))

simsIN  <- 1; simlabIN <- "Persistence"
dlt_SSB <- suppressMessages(rbind(dlt_SSB, get_SSBESM(s = s, lab = "yrpls0_mnhind",yrIN = thisYr+0,
                                                      sims=simsIN,simlab=simlabIN)))
dlt_SSB <- suppressMessages(rbind(dlt_SSB, get_SSBESM(s = s, lab = "yrpls1_mnhind",yrIN = thisYr+1,
                                                      sims=simsIN,simlab=simlabIN)))
dlt_SSB <- suppressMessages(rbind(dlt_SSB, get_SSBESM(s = s, lab = "yrpls2_mnhind",yrIN = thisYr+2,
                                                      sims=simsIN,simlab=simlabIN)))
dlt_SSB <- suppressMessages(rbind(dlt_SSB, get_SSBESM(s = s, lab ="yrpls10_mnhind",yrIN = thisYr+9:11,
                                                      sims=simsIN,simlab=simlabIN)))
dlt_SSB <- suppressMessages(rbind(dlt_SSB, get_SSBESM(s = s, lab = "yr2050_mnhind",yrIN = 2040:2060,
                                                      sims=simsIN,simlab=simlabIN)))
dlt_SSB <- suppressMessages(rbind(dlt_SSB, get_SSBESM(s = s, lab = "yr2080_mnhind",yrIN = 2080:2100,
                                                      sims=simsIN,simlab=simlabIN)))

simsIN  <- which(scens =="ssp126"); simlabIN <- "High mitigation"
dlt_SSB <- suppressMessages(rbind(dlt_SSB, get_SSBESM(s = s, lab = "yrpls0_26", yrIN = thisYr+0,
                                                      sims=simsIN,simlab=simlabIN)))
dlt_SSB <- suppressMessages(rbind(dlt_SSB, get_SSBESM(s = s, lab = "yrpls1_26", yrIN = thisYr+1,
                                                      sims=simsIN,simlab=simlabIN)))
dlt_SSB <- suppressMessages(rbind(dlt_SSB, get_SSBESM(s = s, lab = "yrpls2_26", yrIN = thisYr+2,
                                                      sims=simsIN,simlab=simlabIN)))
dlt_SSB <- suppressMessages(rbind(dlt_SSB, get_SSBESM(s = s, lab = "yrpls10_26", yrIN = thisYr+9:11,
                                                      sims=simsIN,simlab=simlabIN)))
dlt_SSB <- suppressMessages(rbind(dlt_SSB, get_SSBESM(s = s, lab = "yr2050_26",yrIN = 2040:2060,
                                                      sims=simsIN,simlab=simlabIN)))
dlt_SSB <- suppressMessages(rbind(dlt_SSB, get_SSBESM(s = s, lab = "yr2080_26",yrIN = 2080:2100,
                                                      sims=simsIN,simlab=simlabIN)))

simsIN  <- which(scens%in%c("rcp45")); simlabIN <- "Med mitigation"
dlt_SSB <- suppressMessages(rbind(dlt_SSB, get_SSBESM(s = s, lab = "yrpls0_45", yrIN = thisYr+0,
                                                      sims=simsIN,simlab=simlabIN)))
dlt_SSB <- suppressMessages(rbind(dlt_SSB, get_SSBESM(s = s, lab = "yrpls1_45", yrIN = thisYr+1,
                                                      sims=simsIN,simlab=simlabIN)))
dlt_SSB <- suppressMessages(rbind(dlt_SSB, get_SSBESM(s = s, lab = "yrpls2_45", yrIN = thisYr+2,
                                                      sims=simsIN,simlab=simlabIN)))
dlt_SSB <- suppressMessages(rbind(dlt_SSB, get_SSBESM(s = s, lab = "yrpls10_45", yrIN = thisYr+9:11,
                                                      sims=simsIN,simlab=simlabIN)))
dlt_SSB <- suppressMessages(rbind(dlt_SSB, get_SSBESM(s = s, lab = "yr2050_45", yrIN = 2040:2060,
                                                      sims=simsIN,simlab=simlabIN)))
dlt_SSB <- suppressMessages(rbind(dlt_SSB, get_SSBESM(s = s, lab = "yr2080_45",yrIN = 2080:2100,
                                                      sims=simsIN,simlab=simlabIN)))

simsIN  <- which(scens%in%c("rcp85","ssp585")); simlabIN <- "Low mitigation"
dlt_SSB <- suppressMessages(rbind(dlt_SSB, get_SSBESM(s = s, lab = "yrpls0_85", yrIN = thisYr+0,
                                                      sims=simsIN,simlab=simlabIN)))
dlt_SSB <- suppressMessages(rbind(dlt_SSB, get_SSBESM(s = s, lab = "yrpls1_85", yrIN = thisYr+1,
                                                      sims=simsIN,simlab=simlabIN)))
dlt_SSB <- suppressMessages(rbind(dlt_SSB, get_SSBESM(s = s, lab = "yrpls2_85", yrIN = thisYr+2,
                                                      sims=simsIN,simlab=simlabIN)))
dlt_SSB <- suppressMessages(rbind(dlt_SSB, get_SSBESM(s = s, lab = "yrpls10_85", yrIN = thisYr+9:11,
                                                      sims=simsIN,simlab=simlabIN)))
dlt_SSB <- suppressMessages(rbind(dlt_SSB, get_SSBESM(s = s, lab = "yr2050_85",yrIN = 2040:2060,
                                                      sims=simsIN,simlab=simlabIN)))
dlt_SSB <- suppressMessages(rbind(dlt_SSB, get_SSBESM(s = s, lab = "yr2080_85",yrIN = 2080:2100,
                                                      sims=simsIN,simlab=simlabIN)))

dlt_SSB$simlab2 <- dlt_SSB$simlab
dlt_SSB$simlab2 <- gsub("Persistence","No climate change",dlt_SSB$simlab2)
dlt_SSB$simlab  <-  factor(dlt_SSB$simlab,levels=c("Persistence",
                                                 "High mitigation",
                                                 "Med mitigation",
                                                 "Low mitigation",
                                                 "all"))

dlt_SSB$simlab2 <-  factor(dlt_SSB$simlab2,levels=c("No climate change",
                                                 "High mitigation",
                                                 "Med mitigation",
                                                 "Low mitigation",
                                                 "all"))

dlt_SSB
dlt_SSB$spnum   <- dlt_SSB$species
spnames  <- splist[1:3]
dlt_SSB$species <- factor(spnames[dlt_SSB$spnum],levels =spnames)
gg <- viridis::viridis(3,option ="rocket")[1]

p_delta <- ggplot(dlt_SSB%>%filter(MSMmode==0,year>thisYr, simlab!="Persistence",simlab!="all"))+
  geom_col(aes(x=simlab2,y=mn_deltaP*100,fill =simlab))+
  geom_point(aes(x=simlab2,y=mn_deltaP*100),color=gg)+
  geom_errorbar(aes(x=simlab2, 
                    ymin=lower_deltaP*100,
                    ymax=upper_deltaP*100),size=.9,color=gg, width = 0.6)+
  facet_grid(species~year,scales="free_y")+
  scale_fill_viridis_d(begin =.8,end=.3,direction=1,name="Climate Scenario\n(CO2 Mitigation Level)",option ="mako")+ theme_minimal()+
  theme(axis.text.x=element_blank())+
  xlab("")+ylab(paste("% Change in SSB relative to no climate change (Persistence)\n"))

getwd()
png(file=file.path(figs_fldr,"deltaSSB.png"),units="in",width=6,height = 9,res=350)
#png(file=file.path("docs/MSM_Assessment/FigsdeltaSSB.png"),units="in",width=6,height = 9,res=350)
print(p_delta)
dev.off()

gg<-viridis::viridis(3,option ="rocket")[1]

timeblocks <- c( thisYr+1,thisYr+2,thisYr+10, 2050, 2080)
delta      <- c(0,(timeblocks[-1]-timeblocks[-length(timeblocks)] ))
timeblocks_low  <- round(timeblocks-delta/2)
timeblocks_high <- round(timeblocks+delta/2)
timeblocks <- data.frame(timeblock = timeblocks,delta,timeblocks_low,timeblocks_high)

tb_fun <- function(year, timeblocksIN =timeblocks){
  if(year<min(timeblocksIN$timeblocks_low))
    return(0)
  if(year>max(timeblocksIN$timeblocks_high))
    return(max(timeblocksIN$timeblock))
  if(year<=max(timeblocksIN$timeblocks_high)& year>=min(timeblocksIN$timeblocks_low))
     return(timeblocksIN%>%filter(year<=timeblocks_high,year>=timeblocks_low)%>%select(timeblock)%>%as.numeric())
}


tt0 <- data.frame(m_0_1_3$ts$meta,m_0_FUT$ts$SSB)
colnames(tt0) <- c(colnames(m_0_1_3$ts$meta),colnames(m_0_FUT$ts$SSB))

tt2 <- data.frame(m_2_1_3$ts$meta,m_2_FUT$ts$SSB)
colnames(tt2) <- c(colnames(m_2_1_3$ts$meta),colnames(m_2_FUT$ts$SSB))

tmp0 <- reshape2::melt(tt0,id.var=colnames(m_0_1_3$ts$meta),
                       variable.name = "year",value.name = "SSB")
tmp2 <- reshape2::melt(tt2,id.var=colnames(m_2_1_3$ts$meta),
                       variable.name = "year",value.name = "SSB")

fut_SSB <- rbind(tmp0,tmp2)
fut_SSB$year <- as.numeric(as.character(fut_SSB$year))

sim_meta<-data.frame(fut_simulation = 1:length(gcmscen),scen=scens,gcmscen=gcmscen)
scen_levels <- c("Persistence",
                "High mitigation",
                "Med mitigation",
                "Low mitigation",
                "all")
sim_meta$simlab = factor("Low mitigation",levels=scen_levels)

sim_meta$simlab[which(sim_meta$scen=="ssp126")] <- factor("High mitigation",levels=scen_levels)
sim_meta$simlab[which(sim_meta$scen=="rcp45")] <- factor("Med mitigation",levels=scen_levels)
sim_meta$simlab[which(sim_meta$scen=="mnhind")] <- factor("Persistence",levels=scen_levels)
sim_meta$linetype <- 1

fut_SSB2 <- fut_SSB%>%filter(B_target!=0)%>%rowwise()%>%mutate(timeblock = tb_fun(year, timeblocksIN =timeblocks))
thiyrSSB <- fut_SSB2%>%filter(year==thisYr)%>%select(simMode,MSMmode,species, B_target, SSB_thisYr= SSB)%>%distinct()
SSB_persistence <- fut_SSB2%>%filter(fut_simulation==1)%>%select(simMode,MSMmode,species, B_target,year, SSB_persistence = SSB)

fut_SSB3 <- fut_SSB2%>%left_join(thiyrSSB)%>%
  left_join(SSB_persistence)%>%
  mutate(deltaSSB=(SSB-SSB_persistence)/SSB_persistence,
         deltaSSBthisYr=(SSB-SSB_thisYr)/SSB_thisYr)%>%left_join(sim_meta)%>%data.frame()

fut_SSB3%>%group_by(MSMmode,species,timeblock)%>%summarize(SSB_mn = mean(SSB,na.rm=T))
fut_SSB3$species <- spnames[fut_SSB3$species]
fut_SSB3$species <- factor(fut_SSB3$species, levels = spnames)

fut_SSB3$timeblock <- as.numeric(fut_SSB2$timeblock)

#fut_SSB2$timeblock <- factor(fut_SSB2$timeblock ,levels=sort(unique(fut_SSB2$year)))

ssB_sub0 <- fut_SSB3%>%filter(MSMmode==0,year>thisYr,simlab!="Persistence",simlab!="all")
ssB_sub2 <- fut_SSB3%>%filter(MSMmode==2,year>thisYr,simlab!="Persistence",simlab!="all")

bwidth <- 6
p_delta_ts <- ggplot(ssB_sub0)+
  geom_line(aes(x=year,y=deltaSSB, color=simlab, linetype = gcmscen),alpha=.4)+
  geom_hline(yintercept=0,color="grey",linetype="dashed")+
  geom_boxplot(data=ssB_sub0%>%filter(timeblock==timeblocks[2,1],!is.na(timeblock)), 
               aes(x = timeblock, y =deltaSSB, color=simlab, fill = simlab),alpha =0.7,width=bwidth)+
  geom_boxplot(data=ssB_sub0%>%filter(timeblock==timeblocks[3,1],!is.na(timeblock)), 
               aes(x = timeblock, y =deltaSSB, color=simlab, fill = simlab),alpha =0.7,width=bwidth)+
  geom_boxplot(data=ssB_sub0%>%filter(timeblock==timeblocks[4,1],!is.na(timeblock)), 
               aes(x = timeblock, y =deltaSSB, color=simlab, fill = simlab),alpha =0.7,width=bwidth)+
  geom_boxplot(data=ssB_sub0%>%filter(timeblock==timeblocks[5,1],!is.na(timeblock)), 
               aes(x = timeblock, y =deltaSSB, color=simlab, fill = simlab),alpha =0.7,width=bwidth)+
  facet_grid(species~.,scales="free_y")+
  theme_minimal()+
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d(begin =.1,end=.8,direction=1,name="Climate scenario",option ="mako",alpha=.5)+ 
  scale_color_viridis_d(begin =.1,end=.8,direction=1,name="Climate scenario",option ="mako")+ 
  scale_linetype_manual(values= sim_meta$linetype) +
  guides(linetype = "none")+
  xlab("")+ylab("% Change in SSB relative to no climate change\n")

ssB_sub0 <- fut_SSB3%>%filter(MSMmode==0,year>thisYr,simlab!="all")
ssB_sub2 <- fut_SSB3%>%filter(MSMmode==2,year>thisYr,simlab!="all")

p_delta_tsthiYr <- ggplot(ssB_sub0)+
  geom_line(aes(x=year,y=deltaSSBthisYr, color=simlab, linetype = gcmscen),alpha=.4)+
  geom_hline(yintercept=0,color="grey",linetype="dashed")+
  geom_boxplot(data=ssB_sub0%>%filter(timeblock==timeblocks[2,1],!is.na(timeblock)), 
               aes(x = timeblock, y =deltaSSBthisYr, color=simlab, fill = simlab),alpha =0.7,width=bwidth)+
  geom_boxplot(data=ssB_sub0%>%filter(timeblock==timeblocks[3,1],!is.na(timeblock)), 
               aes(x = timeblock, y =deltaSSBthisYr, color=simlab, fill = simlab),alpha =0.7,width=bwidth)+
  geom_boxplot(data=ssB_sub0%>%filter(timeblock==timeblocks[4,1],!is.na(timeblock)), 
               aes(x = timeblock, y =deltaSSBthisYr, color=simlab, fill = simlab),alpha =0.7,width=bwidth)+
  geom_boxplot(data=ssB_sub0%>%filter(timeblock==timeblocks[5,1],!is.na(timeblock)), 
               aes(x = timeblock, y =deltaSSBthisYr, color=simlab, fill = simlab),alpha =0.7,width=bwidth)+
  facet_grid(species~.,scales="free_y")+
  theme_minimal()+
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d(begin =.1,end=.8,direction=1,name="Climate scenario",option ="mako",alpha=.5)+ 
  scale_color_viridis_d(begin =.1,end=.8,direction=1,name="Climate scenario",option ="mako")+ 
  scale_linetype_manual(values= sim_meta$linetype) +
  guides(linetype = "none")+
  xlab("")+ylab(paste("% Change in SSB relative to",thisYr,"\n"))

getwd()
png(file=file.path(figs_fldr,"delta_tsthiYr.png"),units="in",width=9,height = 7,res=350)
print(p_delta_tsthiYr)
dev.off()


getwd()
png(file=file.path(figs_fldr,"delta_ts.png"),units="in",width=9,height = 7,res=350)
print(p_delta_ts)
dev.off()


getwd()
png(file=file.path(figs_fldr,"deltaSSB.png"),units="in",width=6,height = 9,res=350)
print(p_delta)
dev.off()

getProb2 <- function(mn,sd,div,gt="one",prob=.90,plotit=FALSE ){
  # gt=two gives upper and lower
  # gt = one give gives the value of x where cuml = prob
  tt    <- rnorm(1e4,mn,sd)/div
  tt2   <- density(tt)
  if(gt=="two") aa <- (1-prob)/2
  if(gt=="one") aa <- prob
  cuml  <- (cumsum(tt2$y)/sum(tt2$y))
  tt2$x[cuml<=(1-aa)][1]
  out<-NA
  if(gt=="two") out  <-  c(tt2$x[rev(which(cuml<=aa))[1]],tt2$x[rev(which(cuml<=(1-aa)))[1]])
  if(gt=="one") out  <-  tt2$x[(which(cuml>=aa))[1]]
  if(plotit) {
    plot(tt2$x,cuml)
    abline(h=aa)
    abline(v=out)
  }
  return(out)
  
}
getProb3 <- function(s=1,m=2,datIN =dlt_SSB,
                     labIN ="yrpls2_all",useSE=T, mult=1,
                     divlab = "now",gt="one",
                     prob=.90,plotit=FALSE ){
  # gt=two gives upper and lower
  # gt = one give gives the value of x where cuml = prob
  
  
  subdat <- datIN%>%filter(spnum==s,MSMmode ==m)%>%select(mn,sd,se,lab)
  
  mn <- as.numeric(subdat%>%filter(lab==labIN)%>%select(mn) )
  sd <- as.numeric(subdat%>%filter(lab==labIN)%>%select(sd) )*mult
  div<- as.numeric(subdat%>%filter(lab==divlab)%>%select(mn))
  if(useSE)  
    sd <- as.numeric(subdat%>%filter(lab==labIN)%>%select(se) )*mult
  tt    <- rnorm(1e4,mn,sd)/div
  tt2   <- density(tt)
  if(gt=="two") aa <- (1-prob)/2
  if(gt=="one") aa <- prob
  cuml  <- (cumsum(tt2$y)/sum(tt2$y))
  tt2$x[cuml<=(1-aa)][1]
  out<-NA
  if(gt=="two") out  <-  c(tt2$x[rev(which(cuml<=aa))[1]],tt2$x[rev(which(cuml<=(1-aa)))[1]])
  if(gt=="one") out  <-  tt2$x[(which(cuml>=aa))[1]]
  if(plotit) {
    plot(tt2$x,cuml)
    abline(h=aa)
    abline(v=out)
  }
  if(any(out<0))
    out[out<0]<-0
  return(out)
  
}

getProb  <- function(s=1,m=2,datIN =dlt_SSB,
                     labIN ="yrpls2_all", 
                     prcnt=.1,useSE=T,mult=1,
                     limlab = "now",gt=TRUE,
                     prob=.90,plotit=FALSE ){
  
  subdat <- datIN%>%filter(spnum==s,MSMmode ==m)%>%select(mn,sd,se,lab)
  
  mn <- as.numeric(subdat%>%filter(lab==labIN)%>%select(mn) )
  sd <- as.numeric(subdat%>%filter(lab==labIN)%>%select(sd) )*mult
  lim<- as.numeric(subdat%>%filter(lab==limlab)%>%select(mn))
  if(useSE)  
    sd <- as.numeric(subdat%>%filter(lab==labIN)%>%select(se) )*mult
  tt   <- rnorm(1e4,mn,sd)
  tt2  <- density(tt)
  if(gt){
    out <- sum(tt2$y[tt2$x>=lim])/sum(tt2$y)
  }else{
    out <- sum(tt2$y[tt2$x<=lim])/sum(tt2$y)
  }
  return(out)
}

getProb3(s=1,m=2,labIN ="yrpls2_all",gt="one", prob=.95 )
getProb3(s=1,m=2,labIN ="yrpls2_all",gt="one", prob=.95 )
getProb3(s=1,m=2,labIN ="yrpls2_all",gt="two", prob=.60 )
# there is a 50/50 chance SSB will decline by 58% in year 2

prcnt  <- seq(0,1,.01)
probb  <- prcnt*0
for(i in 1:length(prcnt)) 
  probb[i]  <- getProb(s=1,m=2,datIN =dlt_SSB,
                       labIN ="yrpls2_all", 
                       prcnt=prcnt[i],
                       limlab = "now",gt=TRUE,
                       prob=.90,plotit=FALSE )



getsub <- function(tt=F40_1_3[[2]][[1]],scn=1,ftyr=nyrs_fut,cr="1.9",sp=1){
  rr <- which(tt$species%in%sp&tt$Scenario%in%scn&tt$future_year%in%ftyr&tt$Control_rule%in%cr)
  return(tt[rr,])
}

mk_row  <- function(cnm="targetSSB0",dat=B0_list,rr=1:3,roundn=0){
  ssm  <-  dat[[1]][rr,cnm]
  msm  <-  dat[[2]][rr,cnm]
  return ( paste0(formatC(  round(as.vector(rbind(ssm,msm)),roundn)  , 
                            format="d", big.mark=","),collapse="|"))
}

# 2019 catch is the catch during 2019 
yrs         <- as.numeric(names(m_0_1_3$ts$catch))
maxABCrow_1 <- paste0( formatC(round(as.vector(rbind(
  m_0_1_3$ts$catch[,which(yrs==thisYr+1)][4:6],
  m_2_1_3$ts$catch[,which(yrs==thisYr+1)][4:6]))), 
  format="d", big.mark=","),collapse="|")
maxABCrow_2 <- paste0( formatC(round(as.vector(rbind(
  m_0_1_3$ts$catch[,which(yrs==thisYr+2)][4:6],
  m_2_1_3$ts$catch[,which(yrs==thisYr+2)][4:6]))), 
  format="d", big.mark=","),collapse="|")

mod             <- ceattle_0
tmptab          <-  rbind(
  fsh_sel_like=as.numeric(unlist(mod[grep("fsh_sel_like",names(mod))])),
  fsh_age_like=as.numeric(unlist(mod[grep("fsh_age_like",names(mod))])),
  fsh_cat_like=as.numeric(unlist(mod[grep("fsh_cat_like",names(mod))])),
  srv_sel_like=as.numeric(unlist(mod[grep("srv_sel_like",names(mod))])),
  srv_age_like=as.numeric(unlist(mod[grep("srv_age_like",names(mod))])),
  srv_bio_like=as.numeric(unlist(mod[grep("srv_bio_like",names(mod))]))        )

colnames(tmptab)  <-  paste0("sp",1:3)
loglike0          <-  data.frame(t(tmptab));rownames(loglike0)<-paste0("sp",1:3)
mod               <-  ceattle_2
tmptab            <-  rbind(
  fsh_sel_like=as.numeric(unlist(mod[grep("fsh_sel_like",names(mod))])),
  fsh_age_like=as.numeric(unlist(mod[grep("fsh_age_like",names(mod))])),
  fsh_cat_like=as.numeric(unlist(mod[grep("fsh_cat_like",names(mod))])),
  srv_sel_like=as.numeric(unlist(mod[grep("srv_sel_like",names(mod))])),
  srv_age_like=as.numeric(unlist(mod[grep("srv_age_like",names(mod))])),
  srv_bio_like=as.numeric(unlist(mod[grep("srv_bio_like",names(mod))]))        )

loglike2          <- data.frame(t(tmptab));rownames(loglike2)<-paste0("sp",1:3)
mod               <- ceattle_0
cor_table_0       <- data.frame(rbind(
  srvyB=c(
    cor( as.numeric(mod["srv_bio_1"][[1]]),as.numeric(mod["srv_bio_hat_1"][[1]]) ),
    cor( as.numeric(mod["srv_bio_2"][[1]]),as.numeric(mod["srv_bio_hat_2"][[1]]) ),
    cor( as.numeric(mod["srv_bio_3"][[1]]),as.numeric(mod["srv_bio_hat_3"][[1]]) )),
  srvyA=c(
    cor( as.numeric(mod["srv_age_obs_1"][[1]]),as.numeric(mod["srv_age_hat_1"][[1]]) ),
    cor( as.numeric(mod["srv_age_obs_2"][[1]]),as.numeric(mod["srv_age_hat_2"][[1]]) ),
    cor( as.numeric(mod["srv_age_obs_3"][[1]]),as.numeric(mod["srv_age_hat_3"][[1]]) )),
  Cnum=c(
    cor( as.numeric(mod["tc_biom_obs_1"][[1]]),as.numeric(mod["obs_catch_hat_1"][[1]]) ),
    cor( as.numeric(mod["tc_biom_obs_2"][[1]]),as.numeric(mod["obs_catch_hat_2"][[1]]) ),
    cor( as.numeric(mod["tc_biom_obs_3"][[1]]),as.numeric(mod["obs_catch_hat_3"][[1]]) )) ))

mod           <- ceattle_2
cor_table_2   <- data.frame(rbind(
  srvyB=c(
    cor( as.numeric(mod["srv_bio_1"][[1]]),as.numeric(mod["srv_bio_hat_1"][[1]]) ),
    cor( as.numeric(mod["srv_bio_2"][[1]]),as.numeric(mod["srv_bio_hat_2"][[1]]) ),
    cor( as.numeric(mod["srv_bio_3"][[1]]),as.numeric(mod["srv_bio_hat_3"][[1]]) )),
  srvyA=c(
    cor( as.numeric(mod["srv_age_obs_1"][[1]]),as.numeric(mod["srv_age_hat_1"][[1]]) ),
    cor( as.numeric(mod["srv_age_obs_2"][[1]]),as.numeric(mod["srv_age_hat_2"][[1]]) ),
    cor( as.numeric(mod["srv_age_obs_3"][[1]]),as.numeric(mod["srv_age_hat_3"][[1]]) )),
  Cnum=c(
    cor( as.numeric(mod["tc_biom_obs_1"][[1]]),as.numeric(mod["obs_catch_hat_1"][[1]]) ),
    cor( as.numeric(mod["tc_biom_obs_2"][[1]]),as.numeric(mod["obs_catch_hat_2"][[1]]) ),
    cor( as.numeric(mod["tc_biom_obs_2"][[1]]),as.numeric(mod["obs_catch_hat_3"][[1]]) )) ))

# calculate ABC for 2019
s<-1
getC_old<-function(s,m=2,y=nyrs,max=F){
  eval(parse(text=paste0("mod<-ceattle_",m) ))
  if(max){
    if(m==0)  FThisYr<-B0_list[[1]]$F40_pls1[s]
    if(m==2)  FThisYr<-B0_list[[2]]$F40_pls1[s]
  }else{
    if(m==0)  FThisYr<-B0_list[[1]]$Fabc[s]
    if(m==2)  FThisYr<-B0_list[[2]]$Fabc[s]           
  }
  eval(parse(text=paste0( "Fage<-(mod$fsh_sel_",s,"[1,1:nages[",s,"]]*FThisYr)" ) ))
  eval(parse(text=paste0( "M<-mod$M1_",s,"[1,1:nages[",s,"]]+mod$M2_",s,"[nyrs,1:nages[",s,"]]")))
  eval(parse(text=paste0( "N2018<-(mod$AvgN_",s,"[",y,",1:nages[",s,"]]*(1-exp(-(M+Fage))))")))
  eval(parse(text=paste0( "CThisYr<-sum((Fage/(M+Fage))*mod$wt_",s,"[",y,",1:nages[",s,"]]*N2018)")))
  return(list(C=CThisYr,N=N2018,Fage=Fage,M=M,F=FThisYr))
}

nscens<- length(unique(B0_list[[1]]$Scen))
getC <- function(s,m=2,y=nyrs,max=F,scenIN = 1:nscens){
  eval(parse(text=paste0("mod<-ceattle_",m) ))
  
  
  # if(max){
  
  if(m==0)  tmpd <-B0_list[[1]]
  if(m==2)  tmpd <-B0_list[[2]]
  tmpd<- tmpd%>%filter(sp==s,Scen%in%scenIN)%>%
    select(sp,Scen,F40_pls1)%>%ungroup()
  FThisYr <-  mean(tmpd$F40_pls1, na.rm=T)
  
  eval(parse(text=paste0( "Fage<-(mod$fsh_sel_",s,"[1,1:nages[",s,"]]*FThisYr)" ) ))
  eval(parse(text=paste0( "M<-mod$M1_",s,"[1,1:nages[",s,"]]+mod$M2_",s,"[nyrs,1:nages[",s,"]]")))
  eval(parse(text=paste0( "N2018<-(mod$AvgN_",s,"[",y,",1:nages[",s,"]]*(1-exp(-(M+Fage))))")))
  eval(parse(text=paste0( "CThisYr<-sum((Fage/(M+Fage))*mod$wt_",s,"[",y,",1:nages[",s,"]]*N2018)")))
  return(list(C=CThisYr,N=N2018,Fage=Fage,M=M,F=FThisYr))
}

CThisYr<-paste0(formatC(round(c(
  getC(s=1,m=0,max=F)$C,
  getC(s=1,m=2,max=F)$C,
  getC(s=2,m=0,max=F)$C,
  getC(s=2,m=2,max=F)$C,
  getC(s=3,m=0,max=F)$C,
  getC(s=3,m=2,max=F)$C)), format="d", big.mark=","),collapse="|")
maxCThisYr<-paste0(formatC(round(c(
  getC(s=1,m=0,max=T)$C,
  getC(s=1,m=2,max=T)$C,
  getC(s=2,m=0,max=T)$C,
  getC(s=2,m=2,max=T)$C,
  getC(s=3,m=0,max=T)$C,
  getC(s=3,m=2,max=T)$C)), format="d", big.mark=","),collapse="|")

# get mean F:
mnF3<-matrix(0,nyrs,2);colnames(mnF3)<-c("ssm","msm");rownames<-years
mnF1<-mnF2<-mnF3<-mnF3
for(i in 1:nyrs){
  mnF1[i,1]<-(ceattle_0$F_1[i,]/ceattle_0$fsh_sel_1[1,])[1]
  mnF1[i,2]<-(ceattle_2$F_1[i,]/ceattle_2$fsh_sel_1[1,])[1]
  
  mnF2[i,1]<-(ceattle_0$F_2[i,]/ceattle_0$fsh_sel_2[1,])[1]
  mnF2[i,2]<-(ceattle_2$F_2[i,]/ceattle_2$fsh_sel_2[1,])[1]
  
  mnF3[i,1]<-(ceattle_0$F_3[i,]/ceattle_0$fsh_sel_3[1,])[1]
  mnF3[i,2]<-(ceattle_2$F_3[i,]/ceattle_2$fsh_sel_3[1,])[1]
}

#temp<-data.frame(read.csv(file.path(assmnt_path,"data/EBS_BT.csv")))
# temp<-data.frame(readxl::read_excel(path=file.path( data_fl,"Observed_temp_EBS18_sst_bt_cpi.xlsx")    ))
#  BT<-temp[,2]; BT_yr<-temp[,1];SST<-temp[,3]
#COVAR_END ##########################################################DO NOT REMOVE THIS LINE OR REC FIT WONT RUN!
BT_yr <- c(1979, 1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)


BT <- c(6.372075002, 5.087207167, 5.434568424, 2.126850051, 4.691742166, 3.670980743, 3.145908557, 2.611948593, 3.338739852, 3.534290914, 3.651650031, 3.86206383, 4.55655677, 3.847455234, 4.436030969, 2.484951454, 3.268703528, 4.714572698, 4.144296788, 4.393868603, 0.324887709, 2.52973977, 2.630605525, 4.293016949, 4.67176984, 4.691855525, 4.481636295, 2.299462813, 3.072058197, 1.833142699, 2.504502317, 2.150342337, 3.244250062, 1.862479153, 3.267526862, 4.621028098, 4.748391589, 6.425755936, 4.270899972, 5.276690982, 6.469073294, 2.980682098, 4.107047486)


SST <- c(5.802183279, 5.279076536, 7.022110798, 3.535843295, 7.616254449, 5.369833601, 4.552120441, 3.838110546, 4.588459735, 4.994935378, 4.770804818, 5.108122444, 6.311992212, 5.015764281, 5.496801332, 3.989581003, 4.624117651, 5.171977653, 6.537115634, 5.855904317, 1.109124363, 3.62657615, 4.177577815, 5.382759039, 6.334227383, 6.526960774, 5.317627569, 3.132711667, 4.12014852, 2.487072256, 3.008215308, 2.998221648, 3.614375218, 3.090409545, 4.367702458, 6.122062252, 5.663540023, 7.910821713, 5.583466221, 6.449992745, 8.086560684, 5.874603142, 4.983759376)

oldD<-c(4.10105,2.516943,3.676969,2.2501,2.8018,2.3213,2.3817,1.8734,
        3.176,2.2496,2.8416,2.331,2.6126,1.9293,2.9354,1.5245,1.6686,
        3.361,2.6293,3.1681,0.9527,2.0855,2.5022,3.1109,3.6863,3.2467,
        3.3043,1.7913,1.7062,1.2461,1.5379,1.5781,2.4104,0.9576,1.7741,
        2.9928,3.3022,4.5,2.6)
romsD<-c(4.10105,2.899531,3.224109,1.924708,2.681245,1.365911,1.188078,
         1.304252,2.747148,1.598101,2.551962,1.962929,1.830883,0.9680234,
         2.945677,1.759007,1.101053,2.596248,1.946634,3.210328,0.8406364,
         2.269684,2.39882,2.61226,3.632114,3.206721,3.320268,2.131563,
         1.474108,1.285678,1.379501,0.9129241,1.662642,0.6849821)
romsSST<-c(8.383647,8.005092,9.220455,6.665765,9.753227,8.994972,5.443338,
           5.715056,6.97778,7.579359,7.969646,8.654622,7.307674,6.591507,
           8.520948,6.32929,7.244779,7.896445,10.32064,8.049729,4.941675,
           8.368113,7.193832,8.621339,8.495763,9.101472,8.729188,6.937627,
           6.469272,4.978557,5.810558,5.303474,5.565965,4.146672,4.146672,
           4.146672,4.146672,4.146672,7.023004368)
if(1==10){
  load(file.path(data_fl,"hind_20201108.Rdata"))
  sub      <- hind%>%select(year,value,var)%>%
    filter(var%in%c("srvy_rep_temp_bottom5m","srvy_rep_temp_surface5m"))
  romsT    <- dcast(sub, year~var)
  romsT    <- romsT[!is.na(romsT[,2]),]
  
  #RomsYr  <- romsBT$year
  T1 <- merge(data.frame(year=min(romsT$year):thisYr,plotd_SST=NA,plotd_BT=NA),
              temp, by.x="year",by.y="YEAR",all.x=T)
  T1 <- merge(T1,romsT,by="year",all.x =T )
  #romsD  <-ROMS_NPZ_covars$aclim_hindcast$BottomTemp
  #romsSST<-ROMS_NPZ_covars$aclim_hindcast$SST_tmp
  
  T1$plotd_SST <- T1$Surface
  T1$plotd_BT  <- T1$Bottom
  
  T1$plotd_SST[is.na( T1$plotd_SST)] <- T1$srvy_rep_temp_surface5m[is.na( T1$plotd_SST)]
  T1$plotd_BT[is.na(T1$plotd_BT)]   <- T1$srvy_rep_temp_bottom5m[is.na( T1$plotd_BT)]
  
  BT    <- T1$plotd_BT
  SST   <- T1$plotd_SST
}
T1 <- data.frame(year = BT_yr, plotd_BT = BT, plotd_SST= SST)
yrlim <-  thisYr
mn    <-  mean(BT[BT_yr<yrlim])
sd    <-  sd(BT[BT_yr<yrlim])
se    <-  sd/length(BT[BT_yr<yrlim])  # 1982-2000 mean

coll<-rep(colors()[320],length(BT))
wrm<-which(BT>=(mn+1*sd));cld<-which(BT<=(mn-1*sd))
coll[wrm]<-colors()[35] ;coll[cld]<-colors()[124]

plotTemp<-function(w=6,h=4, yrlim=2000){
  #dev.new(width=w,height=h)    
  plot(plotd_BT~year,data=T1,pch="*",type="b",cex=1.3,axes=FALSE,xlim=c(1970,thisYr),
       ylim=c(0,8),ylab="Bottom Temperature oC",xlab="Year")
  sd<-sd(T1$plotd_BT[T1$year<yrlim])
  mn<-mean(T1$plotd_BT[T1$year<yrlim])
  
  axis(1);axis(2,las=2)
  axis(1,at=c(1900,2100));axis(2,at=c(-100,100))
  abline(h=mn,lty=1)
  abline(h=mn+c(1,1.95)*sd,lty=c(3,2),col=c("gray","black"))
  abline(h=mn-c(1,1.95)*sd,lty=c(3,2),col=c("gray","black"))
  # points(plotd_BT~year,data=T1,pch="*",type="b",cex=1.5)
  points(plotd_BT~year,data=T1,pch=16,col=coll,cex=1.2)
  m  <- lm(BT~ny, data=data.frame(ny=1:length(T1$year),BT=T1$plotd_BT))
  ms <- lm(BT~ny, data=data.frame(ny=1:length(T1$year),BT=T1$plotd_BT))
  #abline(a=coef(ms)[1],b=coef(ms)[2])
  
  
}

# promMtbl<-plot_propM()
#dev.new(width=w,height=h)   
#plotTemp()
#quartz.save("/Users/kholsman/GitHub_new/CEATTLE/docs/2017_MSMassmnt/Figs/temp.jpg",dpi=300)
#,message=FALSE,include=FALSE


tier3<-function(Fin,Bratio,alpha=0.05){
  maxFabc<-Bratio*0 
  if(any(Bratio>1)) maxFabc[Bratio>1]<-Fin[Bratio>1]
  if(any(alpha<Bratio&Bratio<=1))
    maxFabc[alpha<Bratio&Bratio<=1]<-Fin[alpha<Bratio&Bratio<=1]*
      ((Bratio[alpha<Bratio&Bratio<=1]-alpha)/(1-alpha))
  if(any(Bratio<alpha)) maxFabc[Bratio<alpha]<-0
  return(maxFabc)
}
Bratio1<-c(m_0_1_3$ts$SSB[1,nyrs]/B0_list$setBfvals[1,1],
           m_0_1_3$ts$SSB[2,nyrs]/B0_list$setBfvals[2,1],
           m_0_1_3$ts$SSB[3,nyrs]/B0_list$setBfvals[3,1])

Bratio2<-c(m_2_1_3$ts$SSB[1,nyrs]/B0_list$setBfvals[1,2],
           m_2_1_3$ts$SSB[2,nyrs]/B0_list$setBfvals[2,2],
           m_2_1_3$ts$SSB[3,nyrs]/B0_list$setBfvals[3,2])

B0_list$SSBny<-rbind(
  c(m_0_1_3$ts$SSB[1,nyrs],
    m_0_1_3$ts$SSB[2,nyrs],
    m_0_1_3$ts$SSB[3,nyrs]),
  c(m_2_1_3$ts$SSB[1,nyrs],
    m_2_1_3$ts$SSB[2,nyrs],
    m_2_1_3$ts$SSB[3,nyrs]))
B0_list$Ftarget<-cbind(B0_list[[1]]$F40_pls1[1:3],B0_list[[2]]$F40_pls1[1:3])
B0_list$FABC<-FABC1
B0_list$Ftier3<-rbind(
  tier3(
    Fin=FABC1[1,],
    Bratio=	Bratio1),
  tier3(
    Fin=FABC1[2,],
    Bratio=	Bratio2))



table2row<-function(tabl=B0_list$efctv_B2100ratio,nr=3,comma=FALSE){
  tmpc<-as.numeric(c(tabl[1,],tabl[2,],tabl[3,]))
  if(comma){
    paste0(formatC(round(tmpc,nr), format="d", big.mark=","), collapse ="| ")
  }else{
    paste0(round(tmpc,nr), collapse ="| ")
  }
  
}
# BASED ON ABC for Y+1 projection and F_ABC for Y+2 projections
ABC_ThisYr<-cbind(c(m_0_FUT$ts$catch[,which(yrs==thisYr)][4:6][1],
                    m_2_FUT$ts$catch[,which(yrs==thisYr)][4:6][1]),
                  c(m_0_FUT$ts$catch[,which(yrs==thisYr)][4:6][2],
                    m_2_FUT$ts$catch[,which(yrs==thisYr)][4:6][2]),
                  c(m_0_FUT$ts$catch[,which(yrs==thisYr)][4:6][3],
                    m_2_FUT$ts$catch[,which(yrs==thisYr)][4:6][3]))
ABC_pls1yr<-cbind(c(m_0_FUT$ts$catch[,which(yrs==thisYr+1)][4:6][1],
                    m_2_FUT$ts$catch[,which(yrs==thisYr+1)][4:6][1]),
                  c(m_0_FUT$ts$catch[,which(yrs==thisYr+1)][4:6][2],
                    m_2_FUT$ts$catch[,which(yrs==thisYr+1)][4:6][2]),
                  c(m_0_FUT$ts$catch[,which(yrs==thisYr+1)][4:6][3],
                    m_2_FUT$ts$catch[,which(yrs==thisYr+1)][4:6][3]))
ABC_pls2yr<-cbind(c(m_0_FUT$ts$catch[,which(yrs==thisYr+2)][4:6][1],
                    m_2_FUT$ts$catch[,which(yrs==thisYr+2)][4:6][1]),
                  c(m_0_FUT$ts$catch[,which(yrs==thisYr+2)][4:6][2],
                    m_2_FUT$ts$catch[,which(yrs==thisYr+2)][4:6][2]),
                  c(m_0_FUT$ts$catch[,which(yrs==thisYr+2)][4:6][3],
                    m_2_FUT$ts$catch[,which(yrs==thisYr+2)][4:6][3]))

other_prey      <- Pred_demand_yr_MSM_1 - (rowSums(E_est_all_1) + 
                                             rowSums(E_est_all_2) + rowSums(E_est_all_3))
Eaten_all       <- rowSums(E_est_all_1) + rowSums(E_est_all_2) + rowSums(E_est_all_3) + other_prey
Eaten_2         <- rowSums(E_est_all_1) + rowSums(E_est_all_2) + rowSums(E_est_all_3) + other_prey
Eaten_3         <- rowSums(E_est_all_3) + rowSums(E_est_all_2) + rowSums(E_est_all_3) + other_prey
mn_eaten        <- mean(Eaten_all[1:(nyrs-1)])
rations_age1    <- data.frame(plk= ceattle_2$Consum_1[,1],pcod=ceattle_2$Consum_2[,1],atf=ceattle_2$Consum_3[,1]) 
rations_age1_mn <- matrix( rep(apply(rations_age1,2,mean),nyrs),nyrs,3,byrow=T)
mnrat           <- mean(as.numeric(((rations_age1-rations_age1_mn)/rations_age1_mn)[nyrs,]))


# copy Figures file to main Figs
if(!dir.exists(file.path(assmnt_path,"Figs/R_figures")))
  dir.create(file.path(assmnt_path,"Figs/R_figures"))
file.copy(from=figfile,to=file.path(assmnt_path,"Figs"), recursive = TRUE,overwrite=TRUE)



# "AICtable",TopAIC_Ricker.txt","TopAIC.txt","TopR2_Ricker.txt","TopR2.txt","topricker" 
load_recruitment_results<-function(pth = file.path(run_fl,"runs",run_versionIN,
                                                   paste0(modelnm,"_2/Recruitment_files/Rec_figs"))){
  load(file.path(pth, "recruitment.Rdata"))
  out <-list()
  for(sp in dim(AICtable))
    AICtable[[sp]]$mod <- rownames(AICtable[[sp]])
  for(cout in c("AICtable","TopAIC_Ricker.txt","RecHat",
                "TopAIC.txt","TopR2_Ricker.txt",
                "TopR2.txt","topricker"))
    out[[cout]]<-eval(parse(text=cout))
  return(out)
  
}
pth_2 <- file.path(run_fl,"runs",run_versionIN,paste0(modelnm,"_2/Recruitment_files/Rec_figs"))
pth_0 <- file.path(run_fl,"runs",run_versionIN,paste0(modelnm,"_0/Recruitment_files/Rec_figs"))
rec_2 <- load_recruitment_results(pth = pth_2)
rec_0 <- load_recruitment_results(pth = pth_0)


load( file.path(run_fl,"Figs/ceattle_covars.Rdata"))
load( file.path(run_fl,"Figs/colIT.Rdata"))

sub_var  <- ceattle_covars%>%select(var,var2,season)%>%unique()%>%
  rowwise()%>%mutate(season2 = substr(season,1,2),var3 = paste0("(",season2,")",var2,""))
sub_var$var3 <- gsub("_depthavg","",sub_var$var3)

get_AIC_smry <- function (rec_in,sub_varIN, modeIN ,splistIN=factor(splist,levels = splist)){
  
  AIC <- rbind(rec_in[[1]][[1]]%>%mutate(Species=splistIN[1],mode = modeIN),
               rec_in[[1]][[2]]%>%mutate(Species=splistIN[2],mode = modeIN),
               rec_in[[1]][[3]]%>%mutate(Species=splistIN[3],mode = modeIN))%>%
    #filter(topSet=="**o")%>%
    select(Species,mode, model,R2,aicc,deltaAIC,cumlAIC,rank,names)%>%
    mutate(aicc = round(aicc,3),
           R2=round(R2,2),
           deltaAIC=round(deltaAIC,3),
           cumlAIC = round(cumlAIC,2),Type = "Ricker")
  AIC$Type[grep("_LM",rownames(AIC))] <- "Linear"
  AIC$Type[grep("_BLM",rownames(AIC))] <- "Linear, SSB(y-1)"
  AIC$model2 <- AIC$model
  
  for(i in 1:dim(sub_varIN)[1])
    AIC$model2 <- gsub(sub_varIN$var[i],sub_varIN$var3[i],AIC$model2)
  
  AIC$model2  <- gsub("NE_winds","NEwinds",AIC$model2 )
  AIC$model2  <- gsub("_LM","",AIC$model2 )
  AIC$model2  <- gsub("_BLM","",AIC$model2 )
  AIC$model2  <- gsub("_BLM","",AIC$model2 )
  AIC$model2  <- gsub("_"," + ",AIC$model2 )
  fxn <- which(substr(AIC$model2,nchar(AIC$model2)-1,nchar(AIC$model2))=="+ ")
  AIC$model2[fxn]<-substr(AIC$model2[fxn],1,nchar(AIC$model2[fxn])-3)
  return(AIC%>%relocate(model2, names, Type, R2,aicc,deltaAIC,cumlAIC,Species,mode,rank))
}


AIC_0 <- get_AIC_smry(rec_in = rec_0,sub_varIN = sub_var,
                      modeIN = factor("Single-species",levels =c("Single-species","Multi-species")))
AIC_2 <- get_AIC_smry(rec_in = rec_2,sub_varIN = sub_var,
                      modeIN = factor("Multi-species",levels =c("Single-species","Multi-species")))
AIC <-rbind(AIC_0,AIC_2)
aic_n <- AIC%>%group_by(Species, mode)%>%summarize(n = length(model))%>%cast(Species~mode,value="n")

AIC_0_TopR2_Ricker <- data.frame(names=rec_0$TopR2_Ricker.txt)%>%left_join(AIC_0)%>%mutate(set ="Top R2 Ricker")
AIC_2_TopR2_Ricker <- data.frame(names=rec_2$TopR2_Ricker.txt)%>%left_join(AIC_2)%>%mutate(set ="Top R2 Ricker")

AIC_0_TopAIC_Ricker <- data.frame(names=rec_0$TopAIC_Ricker.txt)%>%left_join(AIC_0)%>%mutate(set ="Top AIC Ricker")
AIC_2_TopAIC_Ricker <- data.frame(names=rec_2$TopAIC_Ricker.txt)%>%left_join(AIC_2)%>%mutate(set ="Top AIC Ricker")

AIC_0_TopAIC <- data.frame(names=rec_0$TopAIC.txt)%>%left_join(AIC_0)%>%mutate(set ="Top AIC (all models)")
AIC_2_TopAIC <- data.frame(names=rec_2$TopAIC.txt)%>%left_join(AIC_2)%>%mutate(set ="Top AIC (all models)")

AIC_0_TopR2 <- data.frame(names=rec_0$TopR2.txt)%>%left_join(AIC_0)%>%mutate(set ="Top R2 (all models)")
AIC_2_TopR2 <- data.frame(names=rec_2$TopR2.txt)%>%left_join(AIC_2)%>%mutate(set ="Top R2 (all models)")

AIC_0_sub1 <- rbind(AIC_0_TopAIC_Ricker,AIC_0_TopR2_Ricker)%>%select(-model,-names)
AIC_2_sub1 <- rbind(AIC_2_TopAIC_Ricker,AIC_2_TopR2_Ricker)%>%select(-model,-names)

AIC_0_sub2 <- rbind(AIC_0_TopAIC,AIC_0_TopR2)%>%select(-model,-names)
AIC_2_sub2 <- rbind(AIC_2_TopAIC,AIC_2_TopR2)%>%select(-model,-names)



cattxt <- paste0("|",paste0(names(AIC_0_TopR2),collapse="|"),"|\n")
i<-0
for(ch in nchar(AIC_0[1,])){
  i <- i +1
  if(i==1)
    add <- c(paste0(rep("-",ch),collpase=""),":|")
  if(i>1)
    add<- c(add,c(paste0(rep("-",ch),collpase=""),":|"))
}
cattxt <- c(cattxt,paste0("|",paste0(add,collapse=""),"\n"))
cattxt <- c(cattxt,"| *Single-species*   |             |\n")

for(i in 1:dim(AIC_0_sub)[1]){
  cattxt <- c(cattxt,  paste0("|",paste0(AIC_0_sub[i,],collapse="|"),"|\n"))
}

cattxt <- c(cattxt,"| *Multi-species*   |             |\n")
for(i in 1:dim(AIC_2_sub)[1]){
  cattxt <- c(cattxt,  paste0("|",paste0(AIC_2_sub[i,],collapse="|"),"|\n"))
}
cattxt_sub <- cattxt

cattxt <- paste0("|",paste0(names(AIC_0),collapse="|"),"|\n")
i<-0
for(ch in nchar(AIC_0[1,])){
  i <- i +1
  if(i==1)
    add <- c(paste0(rep("-",ch),collpase=""),":|")
  if(i>1)
    add<- c(add,c(paste0(rep("-",ch),collpase=""),":|"))
}
cattxt <- c(cattxt,paste0("|",paste0(add,collapse=""),"\n"))

for(i in 1:dim(AIC_0)[1]){
  cattxt <- c(cattxt,  paste0("|",paste0(AIC_0[i,],collapse="|"),"|\n"))
}

cattxt <- c(cattxt,"| *Multispecies*   |             |\n")
for(i in 1:dim(AIC_2)[1]){
  cattxt <- c(cattxt,  paste0("|",paste0(AIC_2[i,],collapse="|"),"|\n"))
}

save.image("assessment.Rdata")

# ```
# 
# ```{r newplots, eval=F, include =FALSE, echo=F}

suppressWarnings(source("../Assessment_setup.R"))

#source(file.path(DIR_main,'R/sub_fun/PLOT_CEATTLE_FUN_new.R'))
source(file.path(DIR_main,'R/sub_scripts/make_summary_plots.R'))
source(file.path(DIR_main,'R/sub_fun/EcoCons_plots.R'))
# load(file.path(run_fl,"data/in/dietdata/diet_p4CEATTLE.Rdata"))
# if(update.figs){
#        sclr <- 1.2
#        jpeg(file=file.path(assmnt_path,"Figs/diet_p.jpg"),
#             width=w*sclr,height=5*sclr, res=resIN, units="in")
#        print(diet_p$plot)
#        dev.off()
#  }

resIN <-350
#plotTemp2(w=6,h=4)
plotTdat <- readxl::read_xlsx(file.path(assmnt_path,"../../data/in/Temperature_2021.xlsx"))
cc <- which ( plotTdat$Year%in%1982:2000)

mnT <- mean(plotTdat$BottomT_use[cc])
sdT <- sd(plotTdat$BottomT_use[cc])
plotTdat$BT_delta2 <- "mean"
plotTdat$BT_delta2[plotTdat$BottomT_use>=(mnT+sdT)] <- "warm"
plotTdat$BT_delta2[plotTdat$BottomT_use<=(mnT-sdT)] <- "cold"
#quartz.save(file=file.path(assmnt_path,"Bottomtemps.pdf"),type="pdf",dpi=500)
sz <-2.4
pT1 <- ggplot() + 
  geom_line(data=plotTdat,aes(x=Year,y =BottomT_use),color="darkblue") +
  
  geom_point(data=plotTdat,aes(x=Year,y =BottomT_use,shape=ROMSNPZ),size=sz*1.2,color="darkblue") +
  geom_point(data=plotTdat,aes(x=Year,y =BottomT_use,colour= BT_delta2,shape=ROMSNPZ),size=sz) +
  geom_hline(yintercept = mnT,color="gray")+
  geom_hline(yintercept = mnT+sdT,color="gray",linetype="dashed")+
  geom_hline(yintercept = mnT-sdT,color="gray",linetype="dashed")+
  scale_color_brewer(palette = "RdYlBu",direction=-1)+theme_minimal()

if(update.figs){
  sclr <- 1.2
  jpeg(file=file.path(assmnt_path,"Figs/BottomT.jpg"),
       width=6*sclr,height=3*sclr, res=resIN, units="in")
  print(pT1)
  dev.off()
}

sclr<-1

jpeg(file=file.path(assmnt_path,"Figs/Mortality_new.jpg"),height=6*sclr, width=5*sclr, units="in",res=350)
print(plot_M2_new(age=1))
dev.off()

jpeg(file=file.path(assmnt_path,"Figs/annual_consumed_index_scaled_new.jpg"),height=6*sclr, width=4*sclr, units="in",res=350)
print(figureA_scaled())
dev.off()

jpeg(file=file.path(assmnt_path,"Figs/annual_consumed_index_new.jpg"),height=6*sclr, width=4*sclr, units="in",res=350)
print(figureA())
dev.off()

jpeg(file=file.path(assmnt_path,"Figs/annual_ration_index.jpg"),height=6*sclr, width=4*sclr, units="in",res=350)
print(figureB())
dev.off()

resIN <-250
p0 <- plot_sel()
if(update.figs){
  sclr <-1.5
  jpeg(file=file.path(assmnt_path,"Figs/selectivity.jpg"),
       width=5*sclr,height=2*sclr, res=resIN, units="in")
  print(p0$p)
  dev.off()
}
p1 <- plot_allBiomass()
if(update.figs){
  sclr <- 1.5
  jpeg(file=file.path(assmnt_path,"Figs/allBiomass.jpg"),
       width=5*sclr,height=3*sclr, res=resIN, units="in")
  print(p1$p)
  dev.off()
}

p2 <- plot_rec()

if(update.figs){
  sclr <- 1.25
  jpeg(file=file.path(assmnt_path,"Figs/recruitment.jpg"),
       width=7*sclr,height=6*sclr, res=resIN, units="in")
  print(p2$p)
  dev.off()
}

for (cfl in c("Rec_fits","RSPLOT")){
  # multispecies figures
  file.copy(from = file.path(run_fl,"runs",run_versionIN,paste0(modelnm,"_2/projections"),paste0(modelnm,"_2_1_3/R_figures/",cfl,".jpg")),
            to = figs_fldr,overwrite =  T)
  file.rename(from = file.path(figs_fldr,paste0(cfl,".jpg")), 
              to = file.path(figs_fldr,paste0(cfl,"_2.jpg")))
  # singlespecies figures
  file.copy(from = file.path(run_fl,"runs",run_versionIN,paste0(modelnm,"_0/projections"),paste0(modelnm,"_0_1_3/R_figures/",cfl,".jpg")),
            to = figs_fldr,overwrite =  T)
  file.rename(from = file.path(figs_fldr,paste0(cfl,".jpg")), 
              to = file.path(figs_fldr,paste0(cfl,"_0.jpg")))
}



i<-0
for(tmpl in c(2,4,5,6,7)){
  i <- i+1
  tmp_top <- as.character(unlist(c(
    rec_2[[1]][[1]]%>%mutate(mod = rownames(rec_2[[1]][[1]]))%>%filter(mod%in%(rec_2[[tmpl]]))%>%select(model),
    rec_2[[1]][[2]]%>%mutate(mod = rownames(rec_2[[1]][[2]]))%>%filter(mod%in%(rec_2[[tmpl]]))%>%select(model),
    rec_2[[1]][[2]]%>%mutate(mod = rownames(rec_2[[1]][[3]]))%>%filter(mod%in%(rec_2[[tmpl]]))%>%select(model),
    
    rec_0[[1]][[1]]%>%mutate(mod = rownames(rec_0[[1]][[1]]))%>%filter(mod%in%(rec_0[[tmpl]]))%>%select(model),
    rec_0[[1]][[2]]%>%mutate(mod = rownames(rec_0[[1]][[2]]))%>%filter(mod%in%(rec_0[[tmpl]]))%>%select(model),
    rec_0[[1]][[2]]%>%mutate(mod = rownames(rec_0[[1]][[3]]))%>%filter(mod%in%(rec_0[[tmpl]]))%>%select(model))))
  if(i==1)
    toplist <- tmp_top
  if(i>1)
    toplist <- c(toplist,tmp_top)
}
toplist <- unique(toplist)
ceattle_covars_list <- unique(ceattle_covars$var)
ceattle_covars_used <- rep(FALSE,length(ceattle_covars_list))
for(cc in 1:length(ceattle_covars_list)){
  
  if(length(grep(ceattle_covars_list[cc],toplist))>0)
    ceattle_covars_used[cc]<-TRUE
}

assmnt_covars <- ceattle_covars_list[ceattle_covars_used]
tt_use <- ceattle_covars
# plot the full set in one panel:
plot_assmnt_vars<- ggplot(tt_use%>% filter(var%in%assmnt_covars))+
  geom_line(aes(x=mnDate,y=mn_val,color= GCM_scen, linetype = basin))+
  labs(title="CEATTLE Indices, delta corrected to the operational hindcast")+
  geom_line(data = tt_use%>%
              filter(GCM_scen=="A: Hindcast",var%in%assmnt_covars) ,
            aes(x=mnDate,y=mn_val,color= GCM_scen, linetype = basin), 
            inherit.aes = FALSE,
            show.legend = TRUE) +theme_minimal()+
  scale_color_manual(values=colIT,name="Scenario")+
  scale_fill_manual(values=colIT,name="Scenario")+ 
  facet_grid(var~scen,scales="free_y")+
  labs(subtitle = "assessment covariates") +
  theme(legend.position="right",
        strip.text = element_text(size = 6))+
  #guides(color=guide_legend(nrow=1, byrow=TRUE))+
  ylab("")
plot_assmnt_vars
sclr <- 1.2
jpeg(filename = file.path(tmpdir,"Figs/assessment_vars.jpg"),
     width=8*sclr,height=10*sclr,units="in",res=250)
print(plot_assmnt_vars)
dev.off()

plot_all[["Spring"]]


#' plot_RS_new
#' 

plot_RSNEW<-function(MSMmode=0,splistIN = factor(splist,levels = splist), addyr = 7, txtsize=2 ){

  if(MSMmode ==0){
    flpth <- file.path(fl_0,"Recruitment_files/RS_fits")
    TopAIC <- AIC_0_TopAIC
    TopR2  <- AIC_0_TopR2
    TopR2_Ricker  <- AIC_0_TopR2_Ricker
    TopAIC_Ricker <- AIC_0_TopAIC_Ricker
    
    mode2 <-"Single-species"
  }else{
    flpth <- file.path(fl_2,"Recruitment_files/RS_fits")
    TopAIC <- AIC_2_TopAIC
    TopR2  <- AIC_2_TopR2
    TopR2_Ricker  <- AIC_2_TopR2_Ricker
    TopAIC_Ricker <- AIC_2_TopAIC_Ricker 
    mode2 <-"Multi-species"
  }
  
  for(s in 1:3){
    
    covarnames <- read.csv(file.path(fl_2,"Recruitment_files/rec_files",paste0(c("Pollock","Cod","Arrowtooth")[s],"covar_names.txt")), header=F,sep=" ", comment.char = "#")
    
    
    nm <- file.path(fl_2,"Recruitment_files/rec_files",paste0(c("Pollock","Cod","Arrowtooth")[s],"_data.dat"))
    tt    <- scan(nm, flush=T, what = character())
    
    covyrs    <- read.csv(file.path(fl_2,"Recruitment_files/rec_files",paste0(c("Pollock","Cod","Arrowtooth")[s],"_data.dat")), 
                         skip=which(tt=="#years"),
                         nrows=  which(tt=="#SSB")-which(tt=="#years")-1,
                         header=F,sep=" ")|>as.numeric()
    
    covars_tmp    <-read.csv(file.path(fl_2,"Recruitment_files/rec_files",paste0(c("Pollock","Cod","Arrowtooth")[s],"_data.dat")), 
                          skip=which(tt=="#covars"),
                          nrows=  which(tt==12345)-which(tt=="#covars")-1,
                          header=F,sep=" ", comment.char = "#")
    
    rownames(covars_tmp) <- covarnames
    colnames(covars_tmp) <- covyrs
    covars_tmp <- t(covars_tmp)
    covars_tmp <- covars_tmp%>%data.frame()%>%mutate(Species =splistIN[s], mode=MSMmode, mode2=mode2, spnm = s)
   
    
    nm <- paste0("RS",s,"_0")
    tmp0 <- read.csv(file=file.path(flpth,paste0("ceattle_recruit",nm,".rep")),header=T,sep=" ")%>%
      mutate(type="mean",Species =splistIN[s], mode=MSMmode, mode2=mode2, spnm = s, 
             model = "mean", names = nm)
    
    
    nm <- TopAIC%>%filter(Species==splistIN[s])
    tmp1 <- read.csv(file=file.path(flpth,paste0("ceattle_recruit",nm%>%select(names)%>%as.character(),".rep")),header=T,sep=" ")%>%
      mutate(type="top AIC",Species =splistIN[s], mode=MSMmode, mode2=mode2, spnm = s, model = nm$model2, names = nm%>%select(names)%>%as.character())
    
    nm <- TopR2%>%filter(Species==splistIN[s])
    tmp2 <- read.csv(file=file.path(flpth,paste0("ceattle_recruit",nm%>%select(names)%>%as.character(),".rep")),header=T,sep=" ")%>%
      mutate(type="top R2",Species =splistIN[s], mode=MSMmode, mode2=mode2, spnm = s, model = nm$model2, names = nm%>%select(names)%>%as.character())
    
    nm <- TopR2_Ricker%>%filter(Species==splistIN[s])
    tmp3 <- read.csv(file=file.path(flpth,paste0("ceattle_recruit",nm%>%select(names)%>%as.character(),".rep")),header=T,sep=" ")%>%
      mutate(type="top R2 Ricker",Species =splistIN[s], mode=MSMmode, mode2=mode2, spnm = s, model = nm$model2, names = nm%>%select(names)%>%as.character())
    
    nm <- TopAIC_Ricker%>%filter(Species==splistIN[s])
    tmp4 <- read.csv(file=file.path(flpth,paste0("ceattle_recruit",nm%>%select(names)%>%as.character(),".rep")),header=T,sep=" ")%>%
      mutate(type="top AIC Ricker",Species =splistIN[s], mode=MSMmode, mode2=mode2, 
             spnm = s, model = nm$model2, names = nm%>%select(names)%>%as.character())
   
     rec_tmp <-rbind(tmp1,tmp2,tmp3,tmp4)
     covlist <- unique(rec_tmp$names)
     for(cc in 1:length(covlist)){
       cc_num_tmp <- suppressWarnings(  as.numeric(strsplit(unlist(strsplit(covlist[cc],paste0("RS",s)))[-1],"_")[[1]][-1]))
       if(cc ==1){
         cc_num <- cc_num_tmp
       }else{
         cc_num <- c(cc_num,cc_num_tmp)
       }
       
     }
     cc_num <- na.omit(cc_num) |> as.numeric()|> unique()
     rec_covar_tmp <- data.frame(Species =splistIN[s], mode=MSMmode, 
                                 mode2=mode2, spnm = s,Year = covyrs,
                                 covars%>%select(all_of(names(covars)[cc_num])))
     rec_covar_tmp <- rec_covar_tmp%>%
       pivot_longer(colnames(rec_covar_tmp)[-(1:5)],names_to = "covar", values_to = "val")%>%
       mutate(species = factor(snames[s],levels=snames))%>%data.frame()
     
   if(s==1){
      rec       <- rec_tmp
      mnrec     <- tmp0
      covars    <- covars_tmp
      rec_covar <- rec_covar_tmp
    }else{
      rec       <- rbind(rec,rec_tmp)
      mnrec     <- rbind(mnrec,tmp0)
      covars    <- rbind(covars,covars_tmp)
      rec_covar <- rbind(rec_covar,rec_covar_tmp)
    }
  }
  
  
  mnrec <- mnrec%>%
    dplyr::rename(Recruitment_yr = Rec_years, SSB = SSB.y.1.)%>%
    rowwise()%>%
    mutate(species = factor(snames[spnm],levels=snames),plotname = paste0(type,": ",model))%>%data.frame()
  
  rec <- rec%>%
    dplyr::rename(Recruitment_yr = Rec_years, SSB = SSB.y.1.)%>%
    rowwise()%>%
    mutate(species = factor(snames[spnm],levels=snames),plotname = paste0(type,": ",model))%>%data.frame()
  
  rec_covar <- rec_covar%>%rename(var =covar)%>%left_join(sub_var)%>%data.frame()
  
  levelsIN       <- unique(c("Estimated",c(unique(rec$model),unique(rec_covar$var3))))
  rec$model      <- factor(rec$model, levels = levelsIN)
  rec_covar$var3 <- factor(rec_covar$var3, levels = levelsIN)
  
  
  txt <- rec%>%group_by(species, type, model,plotname)%>%
    summarize(max = log(max(R_obs,na.rm=T)),min = log(min(R_obs,na.rm=T)))%>%mutate(delta = max-min)
  step <- .05
  txt$mult <- rep(.99- c(0,seq(1,3,1))*step,3)
  txt$pos <- txt$max*1.06-((1-txt$mult)*txt$delta)
  txt$plotname2 <- str_wrap(  txt$plotname, width = 70,exdent = 5)
 
  
  p_top <- ggplot(data=rec)+
    geom_point(aes(x = Recruitment_yr, y = log(R_obs), color="Estimated",shape="Estimated"),size=sizeIN*1.7)+
    geom_line(aes(x = Recruitment_yr, y = log(R_obs), color="Estimated"),size=sizeIN/2, linetype ="dashed")+
    geom_line(data=mnrec,aes(x = Recruitment_yr, y = log(R_hat), color="mean RS"),size=sizeIN)+
    
    geom_line(data=rec,aes(x = Recruitment_yr, y = log(R_hat), color=model),size=sizeIN)+
    geom_text(data= txt, aes(label=plotname2,y =pos, x=1980,color=model), vjust = "inward", hjust = "inward",size=txtsize)+
    facet_wrap(~species,scales="free_y",ncol=3)+theme_minimal()+
    scale_color_viridis_d(end=.0,begin=0.9,direction=-1, option = "mako")+
    theme(legend.position = "bottom")+ 
    xlab("")+
    guides(shape = "none",color=guide_legend(title=""))+
    coord_cartesian(xlim = c(1980, thisYr+addyr)) 
  
  p_low <- ggplot(data=rec_covar)+
    geom_line(aes(x = Year, y = val, color=var3),size=sizeIN)+
    facet_wrap(~species,scales="free_y",ncol=3)+theme_minimal()+
    scale_color_viridis_d(end=.0,begin=0.9,direction=-1, option = "mako")+
    xlab("Z-scored value")+
    theme(legend.position = "bottom")+ 
    guides(shape = "none",color=guide_legend(title=""))+
    coord_cartesian(xlim = c(1980, thisYr+addyr))  
  
  return(list(p_top=p_top,p_low=p_low, rec=rec, rec_covar=rec_covar,txt=txt))
}

rec0 <- plot_RSNEW(MSMmode=0,addyr = 2, txtsize=2.5)
rec2 <- plot_RSNEW(MSMmode=2,addyr = 2, txtsize=2.5)


library("gridExtra")


sclr <- 1
jpeg(filename = file.path(tmpdir,"Figs/rec_models_0.jpg"),
     width=10*sclr,height=9*sclr,units="in",res=250)
print( grid.arrange(rec0$p_top, rec0$p_low, heights = c(.3,.2),
                   top = textGrob("Single-species",gp=gpar(fontsize=14,font=2)),
                   ncol = 1, nrow = 2) )
dev.off()

sclr <- 1
jpeg(filename = file.path(tmpdir,"Figs/rec_models_2.jpg"),
     width=10*sclr,height=9*sclr,units="in",res=250)
print( grid.arrange(rec2$p_top, rec2$p_low, heights = c(.3,.2),
                    top = textGrob("Multi-species",gp=gpar(fontsize=14,font=2)),
                    ncol = 1, nrow = 2) )
dev.off()


# generate future changes in SSB for projection summary - calc 95% quantiles for each low and high emissions scenarios
# at time intervals




