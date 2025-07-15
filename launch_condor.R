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
remote_dir <- "ofp-sam-swo-2025-ensemble/SWO_grid"                 # Remote directory for CondorBox (e.g., "MFCLtest")
condor_memory <- "10GB"                                        # Memory request for the Condor job (e.g., "6GB")
condor_disk <- "40GB"
condor_cpus <- 8                                               # CPU request for the Condor job (e.g., 4)
branch <- "parallel"                                              # Branch of git repository to use 

# ---------------------------------------
# Run the job on Condor through CondorBox
# ---------------------------------------

nBatch=60

for (i in 1:nBatch) {
  CondorBox::CondorBox(
    make_options = "ss3",
    remote_user = remote_user,
    remote_host = remote_host,
    remote_dir = paste0(remote_dir, "_Batch_", i),  # _Batch_1, _Batch_2, ... _Batch_60
    github_pat = github_pat,
    github_username = github_username,
    github_org = github_org,
    github_repo = github_repo,
    docker_image = docker_image,
    condor_memory = condor_memory,
    condor_cpus = condor_cpus,
    condor_disk = condor_disk,
    branch = branch, 
    rmclone_script = "no",
    ghcr_login = T,
    condor_environment = paste0("BATCH_COUNT=",nBatch, " BATCH_INDEX=", i, " SS3_OPTIONS=-stopph 2 -nohess")  # BATCH_INDEX=1, 2, 3, ... 60
  )
}

# ----------------------------------------------------------
# Retrieve and synchronise the output from the remote server
# ----------------------------------------------------------

CondorBox::CondorUnbox(
  remote_user = remote_user,         # Remote server username (e.g., "kyuhank")
  remote_host = remote_host,         # Remote server address (e.g., "nouofpsubmit.corp.spc.int")
  remote_dir = remote_dir,           # Remote directory containing the output archive (e.g., "MFCLtest")
  local_git_dir = getwd(),           # Destination directory on the local machine (e.g., getwd())
  remote_output_file = "output_archive.tar.gz", # Name of the output archive (e.g., "output_archive.tar.gz")
  overwrite = F                  # Set to TRUE to overwrite existing files
)
