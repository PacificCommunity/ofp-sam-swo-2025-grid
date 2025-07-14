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
remote_dir <- "ofp-sam-swo-2025-ensemble/run_1407_2"                 # Remote directory for CondorBox (e.g., "MFCLtest")
condor_memory <- "8GB"                                        # Memory request for the Condor job (e.g., "6GB")
condor_disk <- "40GB"
condor_cpus <- 8                                               # CPU request for the Condor job (e.g., 4)
branch <- "parallel"                                              # Branch of git repository to use 

# ---------------------------------------
# Run the job on Condor through CondorBox
# ---------------------------------------

CondorBox( make_options = "ss3",
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
  condor_disk = condor_disk,          # Disk request for the Condor job (e.g., "25GB")
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











CondorBox <- function(
    remote_user,
    remote_host,
    remote_dir,
    github_pat,
    github_username,
    github_org,
    github_repo,
    branch = "main",
    docker_image,
    target_folder = NULL,
    condor_cpus = NULL,
    condor_memory = NULL,
    condor_disk= NULL,
    make_options = "all", # Default make options
    ghcr_login = FALSE,
    remote_os = "linux"   # "linux" (default) or "windows"
) {
  # Helper function to normalize paths for Windows (local side)
  normalize_path <- function(path) {
    if (.Platform$OS.type == "windows") {
      normalizePath(path, winslash = "/", mustWork = FALSE)
    } else {
      path
    }
  }
  
  # Define file names
  clone_script <- "clone_job.sh"
  run_script <- "run_job.sh"
  
  # Ensure paths are Windows-compatible (local side)
  remote_dir <- normalize_path(remote_dir)
  
  # 1. Create the clone_job.sh script
  clone_script_content <- sprintf("
#!/bin/bash
export GITHUB_PAT='%s'
export GITHUB_USERNAME='%s'
export GITHUB_ORGANIZATION='%s'
export GITHUB_REPO='%s'
export GITHUB_BRANCH='%s'
%s

if [[ -n \"$GITHUB_TARGET_FOLDER\" ]]; then
    git init
    git remote add origin https://$GITHUB_USERNAME:$GITHUB_PAT@github.com/$GITHUB_ORGANIZATION/$GITHUB_REPO.git
    git config core.sparseCheckout true
    echo \"$GITHUB_TARGET_FOLDER/\" >> .git/info/sparse-checkout
    git pull origin $GITHUB_BRANCH
else
    git clone -b $GITHUB_BRANCH https://$GITHUB_USERNAME:$GITHUB_PAT@github.com/$GITHUB_ORGANIZATION/$GITHUB_REPO.git
fi
", 
                                  github_pat, github_username, github_org, github_repo, branch,
                                  if (!is.null(target_folder)) sprintf("export GITHUB_TARGET_FOLDER='%s'", target_folder) else "")
  
  # Write the clone script
  writeLines(clone_script_content, con = clone_script, sep = "\n")
  
  # 2. Create the run_job.sh script
  run_script_content <- sprintf("
#!/bin/bash

# Source the clone script to perform the git clone
source %s

# Save the working directory
if [[ -n \"$GITHUB_TARGET_FOLDER\" ]]; then
    WORK_DIR=\"$GITHUB_TARGET_FOLDER\"
else
    WORK_DIR=\"$GITHUB_REPO\"
fi

# Delete the clone script after sourcing it
rm -f %s

# Unset the GitHub PAT
unset GITHUB_PAT

# Change into the working directory and run make
cd \"$WORK_DIR\" || exit 1
echo \"Running make with options: %s\"
make %s

# Archive the directory
cd ..
echo \"Archiving folder: $WORK_DIR...\"
tar -czvf output_archive.tar.gz \"$WORK_DIR\"
", clone_script, clone_script, make_options, make_options)
  
  # Write the run script
  writeLines(run_script_content, con = run_script, sep = "\n")
  
  # 3. Create the Condor submit file
  condor_options <- c()
  if (!is.null(condor_cpus)) {
    condor_options <- c(condor_options, sprintf("request_cpus = %s", condor_cpus))
  }
  if (!is.null(condor_memory)) {
    condor_options <- c(condor_options, sprintf("request_memory = %s", condor_memory))
  }
  
  if (!is.null(condor_disk)) {
    condor_options <- c(condor_options, sprintf("request_disk = %s", condor_disk))
  }
  
  condor_options <- paste(condor_options, collapse = "\n")
  
  submit_file <- "condor_job.submit"
  submit_file_content <- sprintf("
Universe   = docker
DockerImage = %s
Executable = /bin/bash
Arguments  = %s
ShouldTransferFiles = YES
TransferInputFiles = %s, %s
TransferOutputFiles = output_archive.tar.gz
Output     = condor_job.out
Error      = condor_job.err
Log        = condor_job.log
environment = IS_CONDOR_RUN=true
%s
Queue
", 
                                 docker_image, run_script, clone_script, run_script, condor_options)
  
  writeLines(submit_file_content, con = submit_file, sep = "\n")
  
  # 4. Check if the remote directory exists
  message("Checking if the remote directory exists...")
  system(sprintf("ssh %s@%s 'mkdir -p %s'", remote_user, remote_host, remote_dir))
  
  # 5. Transfer scripts and submit file to remote server
  message("Transferring the scripts and submit file to the remote server...")
  system(sprintf("scp %s %s@%s:%s/%s", clone_script, remote_user, remote_host, remote_dir, clone_script))
  system(sprintf("scp %s %s@%s:%s/%s", run_script, remote_user, remote_host, remote_dir, run_script))
  system(sprintf("scp %s %s@%s:%s/%s", submit_file, remote_user, remote_host, remote_dir, submit_file))
  
  # Introduce a delay to ensure the files are written and accessible
  message("Waiting briefly to ensure file transfer completion...")
  Sys.sleep(5)  # Wait for 5 seconds
  
  # 5.5. If ghcr_login option is enabled, configure credential helper and perform docker login on the remote server
  if (ghcr_login) {
    if (tolower(remote_os) == "windows") {
      message("Configuring docker credential helper on remote Windows server...")
      config_cmd <- "mkdir %USERPROFILE%\\.docker && echo \"{\\\"credsStore\\\": \\\"wincred\\\"}\" > %USERPROFILE%\\.docker\\config.json"
      system(sprintf("ssh %s@%s '%s'", remote_user, remote_host, config_cmd))
      login_cmd <- sprintf("echo %s | docker login ghcr.io -u %s --password-stdin", 
                           shQuote(github_pat), github_username)
    } else {
      message("Configuring minimal docker config on remote Linux server to avoid credential helper errors...")
      # minimal config: 빈 JSON 객체를 사용하여, 별도의 credential helper를 사용하지 않도록 함.
      config_cmd <- "mkdir -p /tmp/docker_config && echo '{}' > /tmp/docker_config/config.json"
      system(sprintf("ssh %s@%s '%s'", remote_user, remote_host, config_cmd))
      login_cmd <- sprintf("echo %s | DOCKER_CONFIG=/tmp/docker_config docker login ghcr.io -u %s --password-stdin", 
                           shQuote(github_pat), github_username)
    }
    message("Performing docker login on remote server to bypass pull limits...")
    login_status <- system(sprintf("ssh %s@%s '%s'", remote_user, remote_host, login_cmd))
    if (login_status == 0) {
      message("Docker login on remote server succeeded.")
    } else {
      message("Docker login on remote server failed.")
    }
  }
  
  # 6. Submit the Condor job on the remote server
  message("Submitting the Condor job on the remote server...")
  tryCatch({
    system(sprintf("ssh %s@%s 'cd %s && condor_submit %s'", remote_user, remote_host, remote_dir, submit_file))
    message("Condor job submitted successfully!")
  }, error = function(e) {
    message("Condor submission failed: ", e$message)
  })
  
  # 7. Delete clone_job.sh from the remote server regardless of submission success

  # 8. Clean up local files
  unlink(c(clone_script, run_script, submit_file))
  
  message("Cleanup completed.")
}
