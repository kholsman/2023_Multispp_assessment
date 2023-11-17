#'
#'
#'
#'
#' Assessment_setup.R
#' 
    modelIN         <- "ceattle_23_1"
    compileTPL      <- FALSE
    update.datFiles <- FALSE  # ROMSNPZ updates?
    hind_yearsIN    <- 1979:thisYr
    proj_yearsIN    <- (thisYr+1):2100
    hcrset          <-  1.7
    run_versionIN   <- format(Sys.time(), "%Y%m%d%H%S")
    run_versionIN   <- "Nov_final"
    filenm          <- "assmnt_2023"
    modelnm         <- "assmnt_2023"
    subctl          <- "asmnt2023"

    cat("---------------------------------\n")
    cat("CEATTLE MODEL Assessment setup \n")
    cat("   modelIN:       ",modelIN,"\n")
    cat("   compileTPL:    ",compileTPL,"\n")
    cat("   hind_yearsIN:  ",hind_yearsIN[1],":",rev(hind_yearsIN)[1],"\n")
    cat("   proj_yearsIN:  ",proj_yearsIN[1],":",rev(proj_yearsIN)[1],"\n")
    cat("   hcrset:        ",hcrset,"\n")
    cat("   run_versionIN: ",run_versionIN,"\n")
    cat("   filenm:        ",filenm,"\n")
    cat("   modelnm:       ",modelnm,"\n")
    cat("   subctl:        ",subctl,"\n")
    cat("---------------------------------\n")
    # set up directories
  if(.Platform$OS.type=="unix"){
    git_dir   <- "/Volumes/LaCie/GitHub_cloud"
  }else{
    git_dir   <- "D:/GitHub_cloud/"
  }
     
    #
    cat("git_dir in docs/Assessment_setup set to ", git_dir,"\n")
    # git_dir   <-  git_dirIN   <- file.path(getwd(),"../../../../")
    proj_dir  <-  proj_dirIN  <- (file.path(git_dir,"CEATTLE_projects"))
    projIN    <- "2023_Multispp_assessment" #2021_Multispp_assessment"
    DIR_main  <- DIR_mainIN  <- file.path(git_dir,"CEATTLE")
    
    tmpdir <- getwd()
    setwd(DIR_main)
    # load data, packages, and functions
    source(file.path("R/make.R"))
    
    
    # read in the configure file:
    config    <- read_config("config/default")
    main      <- config$output_dir
    ctl_0     <- paste0(subctl,"_0A")
    ctl_2     <- paste0(subctl,"_2A")
    
    run_fl    <- file.path(proj_dir,projIN)
    out_main  <- file.path(run_fl,"runs",run_versionIN)
    fl_0      <- file.path(run_fl,"runs",run_versionIN,paste0(filenm,"_0"))
    fl_2      <- file.path(run_fl,"runs",run_versionIN,paste0(filenm,"_2"))
    ctl_main0 <- ctl_main2<- file.path(run_fl,"data/in/Control_files")
    #ctl_main2 <- file.path(fl_2,"Control_files")
    data_fl0   <- data_fl2 <- file.path(run_fl,"data/in/dat_input_files")
    #data_fl2   <- file.path(fl_2,"dat_input_files")
    fn_0      <- file.path(data_fl0,"set_FabcFofl.dat")
    fn_2      <- file.path(data_fl2,"set_FabcFofl.dat")

    
    #!! Manually update the spreadsheet with this year's ABC values!
    message(paste0(" !! Don't forget to update the ",file.path(run_fl,"data/in/BRP_compare.xlsx")," with last year's ABC values!! "))
    BRP_tables        <- data.frame(readxl::read_xlsx(file.path(run_fl,"data/in/BRP_compare.xlsx")))
    ABC_tables        <- BRP_tables%>%filter(cat=="ABC_yrpls1")%>%select(-Category)
    ABC_lastyr        <- as.numeric(ABC_tables%>%filter(Year==thisYr-1)%>%select(-Year,-cat))
    # ABC_lastyr        <- cbind(c(2370390, 2801100), c(153291, 165341), c(20197, 23935))
    # ABC_target_lastyr <- cbind(c(2833150, 2410344), c(170725, 166643), c(160698, 167878))
    
    
    setwd(tmpdir)
    
