# Install the CondorBox package from GitHub (force reinstallation if needed)
#remotes::install_github("PacificCommunity/ofp-sam-CondorBox", force = TRUE) ## Force reinstallation if updates are needed

## Clean up the Docker resources interactively if needed
# CondorBox::clean_docker_resources_interactive()

# ---------------------------------------------------------------------------------
# Set variables for the remote server and CondorBox job (ignore if running locally)
# ---------------------------------------------------------------------------------

remote_user <- "kyuhank"                                      # Remote server username (e.g., "kyuhank")
remote_host <- "nouofpsubmit.corp.spc.int"                     # Remote server address (e.g., "nouofpsubmit.corp.spc.int")
#remote_host <- "192.168.45.94"                                # kyuhan's local server
github_pat <- Sys.getenv("GIT_PAT")                           # GitHub Personal Access Token (e.g., ghp_....)
github_username <- "kyuhank"                                  # GitHub username (e.g., "kyuhank")
github_org <- "PacificCommunity"                              # GitHub organisation name (e.g., "PacificCommunity")
github_repo <- "ofp-sam-swo-2025-ensemble"                       # GitHub repository name (e.g., "ofp-sam-docker4mfcl-example")
docker_image <- "ghcr.io/pacificcommunity/ss3-3.30.23.1:v1.2"     # Docker image to use (e.g., "kyuhank/skj2025:1.0.4")
condor_memory <- "10GB"                                        # Memory request for the Condor job (e.g., "6GB")
condor_disk <- "14GB"
condor_cpus <- 2                                               # CPU request for the Condor job (e.g., 4)
branch <- "parallel"                                           # Branch of git repository to use 

# ---------------------------------------
# Run the job on Condor through CondorBox
# ---------------------------------------

nBatch=360              ## 360 jobs
maxBatchIndex=nBatch

#BaseCase="Q_10_127_Diag_inputs"  # Base case name
#BaseCase="Q_10_128_PICTCPUE_inputs"  # Base case name
BaseCase="Q_10_129_EUCPUE_inputs"  # Base case name

remote_dir <- paste0(github_repo, "/", BaseCase, "/")  # Remote directory for the job (e.g., "ofp-sam-docker4mfcl-example/P_10_123_AltMove/"))

for (i in 1:maxBatchIndex) {

CondorBox::CondorBox(
    make_options = "ss3",
    remote_user = remote_user,
    remote_host = remote_host,
    remote_dir = paste0(remote_dir, "Batch_", i),  # _Batch_1, _Batch_2, ... _Batch_60
    github_pat = github_pat,
    github_username = github_username,
    github_org = github_org,
    github_repo = github_repo,
    docker_image = docker_image,
    condor_memory = condor_memory,
    condor_cpus = condor_cpus,
    condor_disk = condor_disk,
    stream_error = "TRUE",  
    branch = branch, 
    rmclone_script = "no",
    ghcr_login = T,
    exclude_slots=c("slot1@nouofpcand27",
                    "slot1@nouofpcand28", 
                    "slot1@nouofpcand29",
                    "slot1@nouofpcand30",
                    "slot_1@suvofpcand26.corp.spc.int",
                    "slot_2@suvofpcand26.corp.spc.int",
                    "slot_3@suvofpcand26.corp.spc.int"),   ## these slots are super slow..
    custom_batch_name = paste0("SWO_Grid_EU_", i),
    condor_environment = list(
      BATCH_COUNT = paste0(nBatch),
      BATCH_INDEX = paste0(i), 
      #SS3_OPTIONS = "-stopph 2 -nohess -cbs 2000000000 -gbs 5000000000",
      SS3_OPTIONS = "-maxfn 500 -cbs 2000000000 -gbs 5000000000",
      nCORES="1",
      VERBOSE = "FALSE",
      BaseCase = BaseCase
    )  # BATCH_INDEX=1, 2, 3, ... 60
  )
  
}


################################
## Delete file (clone_job.sh) ##
################################

for (i in 1:maxBatchIndex) {
  
  CondorBox::BatchFileHandler(
    remote_user   = remote_user,
    remote_host   = remote_host,
    folder_name   = paste0(remote_dir, "Batch_", i),
    file_name     = "clone_job.sh",
    action        = "delete"
  )
  
}
  
#######################################
## Extract grids directly to penguin ##
#######################################

for (i in 1:maxBatchIndex) {
  
  CondorBox::BatchFileHandler(
    remote_user   = remote_user,
    remote_host   = remote_host,
    folder_name   = paste0(remote_dir, "Batch_", i),
    action        = "fetch",
    fetch_dir     = "/run/user/1000/gvfs/sftp:host=penguin.corp.spc.int,user=kyuhank/home/shares/assessments/swo/2025/model_runs/grid",
    extract_archive = TRUE,
    direct_extract = TRUE,
    archive_name    = "output_archive.tar.gz",  # Archive file to extract
    extract_folder  = "ofp-sam-swo-2025-ensemble/grids",  # Folder to extract from the archive
  )
  
}



###############################################
## Extract grids directly to local directory ##
###############################################

for (i in 1:maxBatchIndex) {
  
  CondorBox::BatchFileHandler(
    remote_user   = remote_user,
    remote_host   = remote_host,
    folder_name   = paste0(remote_dir, "Batch_", i),
    action        = "fetch",
    fetch_dir     =  "grids",  # Local directory to fetch the grids
    extract_archive = TRUE,
    direct_extract = TRUE,
    archive_name    = "output_archive.tar.gz",  # Archive file to extract
    extract_folder  = paste0("ofp-sam-swo-2025-ensemble/grids/",BaseCase)
  )
  
}
