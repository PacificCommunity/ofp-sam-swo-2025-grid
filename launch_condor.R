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
    condor_environment = paste0("BATCH_COUNT=",nBatch, " BATCH_INDEX=", i)  # BATCH_INDEX=1, 2, 3, ... 60
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
    condor_disk = NULL,
    make_options = "all", # Default make options
    rmclone_script = "yes", # Default to deleting clone_job.sh after run
    ghcr_login = FALSE,
    remote_os = "linux",   # "linux" (default) or "windows"
    condor_environment = NULL
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
  env_file <- "job_env.txt"
  
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
#!/usr/bin/env bash

# 1. Source the clone script to perform the git clone
source %s

# 2. Load environment variables from job_env.txt (if present)
if [[ -f \"%s\" ]]; then
  # Prefix each line with 'export' and source
  grep -E '^[A-Za-z_][A-Za-z0-9_]*=' \"%s\" \\
    | sed 's/^/export /' > env_exports.sh
  source env_exports.sh
fi

# 3. Save the working directory
if [[ -n \"$GITHUB_TARGET_FOLDER\" ]]; then
    WORK_DIR=\"$GITHUB_TARGET_FOLDER\"
else
    WORK_DIR=\"$GITHUB_REPO\"
fi

# 4. Delete the clone script after sourcing it
rm -f %s

# 5. Unset the GitHub PAT
unset GITHUB_PAT

# 6. Change into the working directory and run make
cd \"$WORK_DIR\" || exit 1
echo \"Running make with options: %s\"
make %s

# 7. Archive the directory
cd ..
echo \"Archiving folder: $WORK_DIR...\"
tar -czvf output_archive.tar.gz \"$WORK_DIR\"
", 
                                clone_script,    # 1st %s: clone_job.sh
                                env_file,        # 2nd %s: job_env.txt
                                env_file,        # 3rd %s: job_env.txt
                                clone_script,    # 4th %s: clone_job.sh
                                make_options,    # 5th %s: make options for echo
                                make_options     # 6th %s: make options for make
  )
  
  # Write the run script
  writeLines(run_script_content, con = run_script, sep = "\n")
  
  if (!is.null(condor_environment)) {
    # condor_environment may be "VAR1=val1 VAR2=val2"
    lines <- strsplit(condor_environment, "\\s+")[[1]]
    writeLines(lines, env_file)
  }
  
  
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
  
  # Process environment variables
  environment_string <- ""
  if (!is.null(condor_environment)) {
    if (is.list(condor_environment)) {
      # Handle list format: list(VAR1 = "value1", VAR2 = "value2")
      env_vars <- sapply(names(condor_environment), function(name) {
        sprintf("%s=%s", name, condor_environment[[name]])
      })
      environment_string <- paste(env_vars, collapse = " ")
    } else if (is.character(condor_environment)) {
      # Handle string format: "VAR1=value1 VAR2=value2"
      environment_string <- condor_environment
    }
  }
  
  # Combine all condor options
  condor_options <- paste(condor_options, collapse = "\n")
  
  # 4. Create HTCondor submit file
  submit_file <- "condor_job.submit"
  submit_file_content <- sprintf("
Universe   = docker
DockerImage = %s
Executable = /bin/bash
Arguments  = %s
ShouldTransferFiles = YES
TransferInputFiles = %s, %s, %s
TransferOutputFiles = output_archive.tar.gz
Output     = condor_job.out
Error      = condor_job.err
Log        = condor_job.log
getenv = True
%s%s
Queue
", 
                                 docker_image, 
                                 run_script, 
                                 clone_script, 
                                 run_script,
                                 env_file,
                                 if(nzchar(environment_string)) sprintf("environment = %s\n", environment_string) else "",
                                 if(nzchar(condor_options)) paste0(condor_options, "\n") else ""
  )
  
  writeLines(submit_file_content, con = submit_file, sep = "\n")
  
  # 4. Check if the remote directory exists
  message("Checking if the remote directory exists...")
  system(sprintf("ssh %s@%s 'mkdir -p %s'", remote_user, remote_host, remote_dir))
  
  # 5. Transfer scripts and submit file to remote server
  message("Transferring the scripts and submit file to the remote server...")
  system(sprintf("scp %s %s@%s:%s/%s", clone_script, remote_user, remote_host, remote_dir, clone_script))
  system(sprintf("scp %s %s@%s:%s/%s", run_script, remote_user, remote_host, remote_dir, run_script))
  system(sprintf("scp %s %s@%s:%s/%s", env_file, remote_user, remote_host, remote_dir, env_file))
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
  if(rmclone_script == "yes") {
    message("Waiting for 20 seconds before deleting clone_job.sh from the remote server...")
    Sys.sleep(20)  # Wait for 10 seconds
    message("Deleting clone_job.sh from the remote server...")
    system(sprintf("ssh %s@%s 'rm -f %s/%s'", remote_user, remote_host, remote_dir, clone_script))
  } else {
    message("Skipping deletion of clone_job.sh from the remote server as per user request.")
  }
  
  # 8. Clean up local files
  unlink(c(clone_script, run_script, submit_file))
  
  message("Cleanup completed.")
}
