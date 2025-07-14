# Install the CondorBox package from GitHub (force reinstallation if needed)
# remotes::install_github("PacificCommunity/ofp-sam-CondorBox", force = TRUE) ## Force reinstallation if updates are needed

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
remote_dir <- "ofp-sam-swo-2025-ensemble/testRun"                 # Remote directory for CondorBox (e.g., "MFCLtest")
condor_memory <- "20GB"                                        # Memory request for the Condor job (e.g., "6GB")
condor_cpus <- 20                                               # CPU request for the Condor job (e.g., 4)
branch <- "parallel"                                              # Branch of git repository to use 

# ---------------------------------------
# Run the job on Condor through CondorBox
# ---------------------------------------

CondorBox::CondorBox( make_options = "ss3",
  remote_user = remote_user,         # Remote server username (e.g., "kyuhank")
  remote_host = remote_host,         # Remote server address (e.g., "nouofpsubmit.corp.spc.int")
  remote_dir = remote_dir,           # Remote directory for CondorBox (e.g., "MFCLtest")
  github_pat = github_pat,           # GitHub Personal Access Token (e.g., ghp_....)
  github_username = github_username, # GitHub username (e.g., "kyuhank")
  github_org = github_org,           # GitHub organisation name (e.g., "PacificCommunity")
  github_repo = github_repo,         # GitHub repository name (e.g., "ofp-sam-docker4mfcl-example")
  docker_image = docker_image,       # Docker image to use (e.g., "kyuhank/skj2025:1.0.4")
  condor_memory = condor_memory,     # Memory request for the Condor job (e.g., "6GB")
  condor_cpus = condor_cpus,          # CPU request for the Condor job (e.g., 4) 
  branch=branch,                      # Branch of git repository to use (default is "main")
  ghcr_login = T                  # Set to TRUE to login to GitHub Container Registry (GHCR)
)

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
