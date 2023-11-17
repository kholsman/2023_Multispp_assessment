#'
#'
#'
#'get_historicalFit.R
#'
#'


# -----------------------------------------------------
# run multi spp in estimation mode:
# -----------------------------------------------------

run_est(
  ex_model    = modelIN,  # executable
  updateModel = TRUE,    # copy the ex_model over to CEATTLE folder? (select TRUE if running non-default models)
  run_version = run_versionIN,  # what is the final meta folder name for the runs? eg. "Nov_final"
  proj_path  = run_fl,  # what is the main path of the project?
  data_path  = file.path(run_fl,"data/in"), # where is the data located in the project directory?
  model_path = file.path(run_fl,"model"),   # where is the model locate in the project directory?
  run_path   = file.path(DIR_main,config$model_path), # what is the folder where the CEATTLE model is located and run?
  multispp   = TRUE,     # run in multispecies mode?
  flnmIN     = filenm, #assmnt_2022  # run or file name for the simulation
  ctl_file   = ctl_2,  #asmnt2022_2A # control file for the simulation
  skip_est   = FALSE,  # skip estimate and go right to plotting? (defunct)
  debug      = debugIT,  # run in debug mode
  plotIT     = FALSE,  # defunct
  DIR_mainIN   = DIR_main, # Main directory for the CEATTLE model
  overwrite  = TRUE)   # overwrite the existing results?

  # -----------------------------------------------------
  # Get M2 vector from multispecies model and 
  # center the M1 vector for single spp on that value
  # -----------------------------------------------------
  
  # get multi spp dat filename
  # fl_2      <- file.path(out_main,paste0(filenm,"_2"))
  #ctlfl          <- paste0(ctl_2,".ctl")
  ctl2           <- read_ctl(file.path(run_fl,"data/in/Control_files",paste0(ctl_2,".ctl")))
  datafile_name  <- ctl2$datafile_name
  msmfl          <- strsplit(datafile_name,".dat")[[1]][1]
  ssfl           <- paste0(substr(msmfl,1,nchar(msmfl)-1),"0.dat")

# load results from Multispp:
load(file.path(out_main,paste0(filenm,"_2"),"results/CEATTLE_results.Rdata"))

# Get Mortality matrix:
M2_1    <- apply(tmp$M2_1,2,mean)
M2_2    <- apply(tmp$M2_2,2,mean)
M2_3    <- apply(tmp$M2_3,2,mean)
MortMat <- list(M2_1=tmp$M2_1,
                M2_2=tmp$M2_2,
                M2_3=tmp$M2_3)
M1      <- list()
M1[[1]] <- round(M2_1+tmp$M1_1[1,],4)
M1[[2]] <- round(M2_2+tmp$M1_2[1,],4)
M1[[3]] <- round(M2_3+tmp$M1_3[1,],4)

meanMortMat <-list(M2_1=M2_1,
                   M2_2=M2_2,
                   M2_3=M2_3)
Mort         <-list(
  M2_Matrixyr   = MortMat,
  meanM2_Matrix = meanMortMat, M1=M1)
save(Mort, file=file.path(fl_2,"results/Mort.Rdata"))

# now replace m1 vector in single spp data file and
# replace the ctl file with new datfilename
dd <- file.path(run_fl,
                "data/in/Control_files",paste0(ctl_0,".ctl"))
if(!file.exists(dd)) file.create(dd)
replace_dat(
  flin  = file.path(run_fl,
                    "data/in/Control_files",paste0(ctl_2,".ctl")),
  flout = file.path(run_fl,
                    "data/in/Control_files",paste0(ctl_0,".ctl")),
  nm    = "datafile_name",
  skip  = 0,
  rplac = list(ssfl))

# make new datafile
replace_dat(
  flin  = file.path(run_fl,"data/in",datafile_name),
  flout = file.path(run_fl,"data/in",ssfl),
  nm    = "M1_base",
  rplac = M1,
  skip  = 1)

#clean up the temp folder: Not needed?
system(paste0("rm -rf ",file.path(DIR_main,"temp")))
dir.create(file.path(DIR_main,"temp"))

# -----------------------------------------------------
# run single spp in estimation mode & plot it
# -----------------------------------------------------

run_est(
  ex_model    = modelIN,  # executable
  updateModel = FALSE,    # updated the executable in the run folder?
  run_version = run_versionIN,  # what is the final meta folder name for the runs?
  data_path  = file.path(run_fl,"data/in"), # where is the data located in the project directory?
  model_path = file.path(run_fl,"model"),   # where is the model located Only used if updateModel = T
  run_path   = file.path(DIR_main,config$model_path), # what is the folder where the CEATTLE model is located and run?
  multispp   = FALSE,     # run in multispecies mode?
  flnmIN     = filenm, #assmnt_2022  # run or file name for the simulation
  ctl_file   = ctl_0,  #asmnt2022_2A # control file for the simulation
  skip_est   = FALSE,  # skip estimate and go right to plotting? (defunct)
  debug      = FALSE,  # run in debug mode
  plotIT     = FALSE,   # defunct
  DIR_mainIN   = DIR_main, # Main directory for the CEATTLE model
  overwrite  = TRUE)   # overwrite the existing results?

# make plots:
# --------------------------------------------
fn <- fl_2
source(file.path(DIR_main,"R/sub_scripts/load_config.R"))
rm(fn)
source(file.path(DIR_main,"R/sub_scripts/PLOT_CEATTLE_EST_new.R"))
message("---------- Historical fits complete ----------\n----------------------------------------------\n")
