#'
#'
#'
#'
#' get_ClimateNaiveABC.R
#' 


#______________________________________________
# Now using ABC as mean catch at 2095-2100, and F40, B40, and B0 from above, project under set C=ABC,
# in order to get F40 in 2018, and F40 in 2019
# Update " dat_input_files/set_catch.dat" files in order to set_val for catch = ABC from step 3
#______________________________________________
#create a data file for each sub scenario

# Load projections
load(file.path(fl_0,"projections",paste0(modelnm,"_0_1_3/results/proj.Rdata")))
m_0_1_3  <- proj ; rm(proj) # loads the proj data file
load(file.path(fl_2,"projections",paste0(modelnm,"_2_1_3/results/proj.Rdata")))
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
parm0_yr1  <- getFparms(mode=0,scen=1,mn=file.path(run_fl,"runs",run_versionIN),flname=filenm,
                        modelname=modelnm,rec =1, hvst=3,hcr=hcrset,yr=1+nyrs,endyr=nyrs,catch=maxABC_0)
parm2_yr1  <- getFparms(mode=2,scen=1,mn=file.path(run_fl,"runs",run_versionIN),flname=filenm,
                        modelname=modelnm,rec =1, hvst=3,hcr=hcrset,yr=1+nyrs,endyr=nyrs,catch=maxABC_0)

parm0_yr2  <- getFparms(mode=0,scen=1,mn=file.path(run_fl,"runs",run_versionIN),flname=filenm,
                        modelname=modelnm,rec =1, hvst=3,hcr=hcrset,yr=2+nyrs,endyr=nyrs,catch=maxABC_0)
parm2_yr2  <- getFparms(mode=2,scen=1,mn=file.path(run_fl,"runs",run_versionIN),flname=filenm,
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

if(1==2){
# Save everything
save(maxABC,file=file.path(file.path(run_fl,"runs",run_versionIN),"maxABC.Rdata"))
save(FABC1,file=file.path(file.path(run_fl,"runs",run_versionIN), "FABC1.Rdata"))
save(FABC2,file=file.path(file.path(run_fl,"runs",run_versionIN), "FABC2.Rdata"))
save.image(file=file.path(file.path(run_fl,"runs",run_versionIN), "assmntStuff.Rdata"))}
