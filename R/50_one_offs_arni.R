# Lorenzen scaler, specifying M for females at age 7

library(r4ss)

copy_dir <- function(from, to, overwrite=FALSE)
{
  if(dir.exists(to))
  {
    if(overwrite)
      unlink(to, recursive=TRUE)
    else
      stop(to, " already exists")
  }
  dir.create(to)
  file.copy(dir(from, full=TRUE), file.path(to, dir(from)), copy.date=TRUE)
  invisible(TRUE)
}

set_natmort <- function(dir, value)
{
  ctl_file <- file.path(dir, "swo2025.ctl")
  dat_file <- file.path(dir, "swo2025.dat")
  ctl <- SS_readctl(ctl_file, datlist=dat_file)
  ctl$MG_parms[rownames(ctl$MG_parms)=="NatM_p_1_Fem_GP_1", "INIT"] <- value
  SS_writectl(ctl, ctl_file, overwrite=TRUE)
}

# M values from Jemery's message on Teams, 7 July 2025
M <- c("05"=0.1501762, "10"=0.1725436, "15"=0.1813481, "20"=0.1926375,
       "50"=0.2648857)
p <- names(M)
dirname <- paste0("M_", p, "th")

for(i in seq_along(M))
{
  copy_dir(file.path("../02_one_offs", "base"),
           file.path("../02_one_offs", dirname[i]), overwrite=TRUE)
  set_natmort(file.path("../02_one_offs", dirname[i]), M[[i]])
}
