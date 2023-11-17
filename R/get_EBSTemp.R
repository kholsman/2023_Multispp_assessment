#'
#'
#'
#'
#'get_EBSTemp.R
#'


# get hindcast from ACLIM server:
# setwd("/Volumes/LaCie/GitHub_cloud/CEATTLE_projects/2022_Multispp_assessment")
# copy dat files from ACLIM2 

file.copy(from  = file.path(from_fldr,"ADMB_datfiles",flhd), 
          to =to_fldr,overwrite = T)

file.copy(from  = file.path(from_fldr,"ADMB_datfiles",flft), 
          to =to_fldr,overwrite = T)

file.copy(from  = file.path(from_fldr,"CEATTLE_indices","ceattle_vars_op.Rdata"), 
          to = to_fldr,overwrite = T)
# file.copy(from  = file.path("../../ACLIM2/Data/out/ADMB_datfiles/Figs","hind.Rdata"), to =to_fldr,overwrite = T)
# 
# load("data/in/dat_input_files/hind.Rdata")
# load("data/in/dat_input_files/fut.Rdata")
load(file.path(to_fldr,"ceattle_vars_op.Rdata"))

# first get Bottom and SST from GAP: (update coldpool package)
# devtools::install_github("afsc-gap-products/coldpool")

# View loaded cold pool index data frame
# coldpool:::cold_pool_index
# coldpool:::ebs_bottom_temperature
# https://github.com/afsc-gap-products/coldpool/blob/main/data/ebs_nbs_temperature_full_area.csv
# https://github.com/afsc-gap-products/coldpool/blob/main/data/survey_replicates_B10K-K20_CORECFS.csv

download.file(destfile = file.path(to_fldr,"survey_replicates_B10K-K20_CORECFS.csv"), 
                  url ="https://raw.githubusercontent.com/afsc-gap-products/coldpool/main/data/survey_replicates_B10K-K20_CORECFS.csv",
         )
download.file(destfile = file.path(to_fldr,"ebs_nbs_temperature_full_area.csv"), 
              url ="https://raw.githubusercontent.com/afsc-gap-products/coldpool/main/data/ebs_nbs_temperature_full_area.csv",
)

survy_obs  <- read.csv(file.path(to_fldr,"ebs_nbs_temperature_full_area.csv"))
survy_roms  <- read.csv(file.path(to_fldr,"survey_replicates_B10K-K20_CORECFS.csv"))

# library(ggplot2)
# library(dplyr)
library(coldpool)
BT <- survy_obs
df_bt <- getValues(BT)
mnBT <- data.frame(
  Temp =  apply(df_bt,2,mean,na.rm=T),
  sd   =  apply(df_bt,2,mean,na.rm=T),
  year = as.numeric(substr(names(apply(df_bt,2,mean,na.rm=T)),10,13)))
mnBT$refmn<-mean(mnBT$Temp[mnBT$year<=2000])
mnBT$refsd<-sd(mnBT$Temp[mnBT$year<=2000])
mnBT$type = "survey BT"
mnBT$region = "SEBS"

sst <- coldpool:::ebs_surface_temperature
df_sst <- getValues(sst)
mnSST <- data.frame(
  Temp =  apply(df_sst,2,mean,na.rm=T),
  sd   =  apply(df_sst,2,mean,na.rm=T),
  year = as.numeric(substr(names(apply(df_sst,2,mean,na.rm=T)),10,13)))
mnSST$refmn<-mean(mnSST$Temp[mnSST$year<=2000])
mnSST$refsd<-sd(mnSST$Temp[mnSST$year<=2000])
mnSST$type = "survey SST"
mnSST$region = "SEBS"

ebsT<-rbind(mnSST,mnBT)

rm(list(c("mnBT","mnSST")))

BT <- coldpool:::nbs_ebs_bottom_temperature
df_bt <- getValues(BT)
mnBT <- data.frame(
  Temp =  apply(df_bt,2,mean,na.rm=T),
  sd   =  apply(df_bt,2,mean,na.rm=T),
  year = as.numeric(substr(names(apply(df_bt,2,mean,na.rm=T)),10,13)))
mnBT$refmn<-mean(mnBT$Temp[mnBT$year<=2000],na.rm=T)
mnBT$refsd<-sd(mnBT$Temp[mnBT$year<=2000],na.rm=T)
mnBT$type = "survey BT"
mnBT$region = "SEBS+NEBS"

sst <- coldpool:::nbs_ebs_surface_temperature
df_sst <- getValues(sst)
mnSST <- data.frame(
  Temp =  apply(df_sst,2,mean,na.rm=T),
  sd   =  apply(df_sst,2,mean,na.rm=T),
  year = as.numeric(substr(names(apply(df_sst,2,mean,na.rm=T)),10,13)))
mnSST$refmn<-mean(mnSST$Temp[mnSST$year<=2000],na.rm=T)
mnSST$refsd<-sd(mnSST$Temp[mnSST$year<=2000],na.rm=T)
mnSST$type = "survey SST"
mnSST$region = "SEBS+NEBS"

ebsT_ns<-rbind(mnSST,mnBT)

ebsT<-rbind(ebsT,ebsT_ns)

rm(list(c("mnBT","mnSST")))

BT <- coldpool:::nbs_ebs_bottom_temperature
df_bt <- getValues(BT)
mnBT <- data.frame(
  Temp =  apply(df_bt,2,mean,na.rm=T),
  sd   =  apply(df_bt,2,mean,na.rm=T),
  year = as.numeric(substr(names(apply(df_bt,2,mean,na.rm=T)),10,13)))
mnBT$refmn<-mean(mnBT$Temp[mnBT$year<=2000],na.rm=T)
mnBT$refsd<-sd(mnBT$Temp[mnBT$year<=2000],na.rm=T)
mnBT$type = "survey BT"
mnBT$region = "NEBS"

sst <- coldpool:::nbs_surface_temperature
df_sst <- getValues(sst)
mnSST <- data.frame(
  Temp =  apply(df_sst,2,mean,na.rm=T),
  sd   =  apply(df_sst,2,mean,na.rm=T),
  year = as.numeric(substr(names(apply(df_sst,2,mean,na.rm=T)),10,13)))
mnSST$refmn<-mean(mnSST$Temp[mnSST$year<=2000],na.rm=T)
mnSST$refsd<-sd(mnSST$Temp[mnSST$year<=2000],na.rm=T)
mnSST$type = "survey SST"
mnSST$region = "NEBS"

ebsT_n <- rbind(mnSST,mnBT)

ebsT <- rbind(ebsT,ebsT_n)
ebsT$type <-factor(ebsT$type,levels=c("survey SST","survey BT"))
sub <- ebsT%>%filter(region%in%c("SEBS"))
sst <-sub%>%filter(type=="survey SST")
bt  <- sub%>%filter(type=="survey BT")

mod_BT <- hind%>% 
  dplyr::filter(var=="temp_bottom5m_Summer")%>%
  dplyr::rename(Temp = mn_val )%>%mutate(type = 'modeled BT')
mod_sst <- hind%>% 
  dplyr::filter(var=="temp_surface5m")%>%
  dplyr::rename(Temp = mn_val )%>%mutate(type = 'modeled SST')

temp_fig <- ggplot()+
  geom_smooth(data=sst,aes(x=year,y=Temp),alpha=.4,
              color="white",method = 'loess',formula= 'y ~ x')+
  geom_line(data=sst,aes(x = year,y=refmn),col="gray",linetype="solid",size=.8)+
  geom_line(data=sst,aes(x = year,y=refmn+refsd),col="gray",linetype="dashed")+
  geom_line(data=sst,aes(x = year,y=refmn-refsd),col="gray",linetype="dashed")+
  geom_line(data=sst,aes(x=year,y=Temp),col="gray40",linetype="solid")+
  geom_point(data=sst,aes(x=year,y=Temp,shape=type),color="gray40",size=2.2)+
  geom_point(data=sst,aes(x=year,y=Temp,shape=type,col=Temp),size=1.5)+
  # 
  # geom_point(data=mod_sst,aes(x=year,y=Temp,shape=type),color="gray40",size=2)+
  # geom_point(data=mod_sst,aes(x=year,y=Temp,shape=type,col=Temp),size=1.5)+
  # 
  geom_smooth(data=bt,aes(x=year,y=Temp),alpha=.4,
              color="white",method = 'loess',formula= 'y ~ x')+
  geom_line(data=bt,aes(x = year,y=refmn),col="gray",linetype="solid",size=.8)+
  geom_line(data=bt,aes(x = year,y=refmn+refsd),col="gray",linetype="dashed")+
  geom_line(data=bt,aes(x = year,y=refmn-refsd),col="gray",linetype="dashed")+
  geom_line(data=bt,aes(x=year,y=Temp),col="gray40")+
  geom_point(data=bt,aes(x=year,y=Temp,shape=type),color="gray40",size=2.2)+
  geom_point(data=bt,aes(x=year,y=Temp,col=Temp,shape=type),size=1.5)+
  
  geom_line(data=mod_BT,aes(x=year,y=Temp),col="gray40",linetype="dashed")+
  geom_point(data=mod_BT,aes(x=year,y=Temp,shape=type),color="gray40",size=2.2)+
  geom_point(data=mod_BT,aes(x=year,y=Temp,col=Temp,shape=type),size=1.5)+
  scale_color_distiller(palette = "Spectral")+
  #facet_grid(type~region)+
  theme_minimal()
temp_fig

png(filename = "Figs/TempPlot.png",units="in",res = 250, height=5,width=7)
# now identify which covars are highly correlated
print(temp_fig)
dev.off()