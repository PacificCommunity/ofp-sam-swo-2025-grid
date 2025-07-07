copy_exe <- function(folder, version)
{
  if(!dir.exists(folder))
    stop("missing directory '", folder, "'")
  file.copy(file.path("exe", version, "ss3"), folder, overwrite=TRUE,
            copy.date=TRUE)
  invisible(NULL)
}

copy_condor <- function(folder)
{
  if(!dir.exists(folder))
    stop("missing directory '", folder, "'")
  file.copy(c("condor/condor.sub", "condor/condor_run.sh"), folder,
            overwrite=TRUE, copy.date=TRUE)
  invisible(NULL)
}

full_submit <- function(folder, version, up=TRUE, ...)
{
  if(up)
  {
    owd <- setwd("..")
    on.exit(setwd(owd))
  }
  copy_exe(folder, version)  # put ss3 into model folder
  copy_condor(folder)        # put condor files into model folder
  condor_submit(folder, ...)
}

full_download <- function(folder, up=TRUE, ...)
{
  dir.create(folder, recursive=TRUE, showWarnings=FALSE)
  condor_download(local.dir=folder, ...)
  unlink(file.path(folder, c("tmp", "var", "_condor_stderr", "_condor_stdout")),
         recursive=TRUE)
}

condor_to_penguin <- function(folder)
{
  # penguin_session <- ssh_connect("penguin")
  # ssh_exec_wait(session, paste("mkdir -p", remote.dir))
  # scp_upload(session, files=Start.tar.gz, to=remote.dir)
}
