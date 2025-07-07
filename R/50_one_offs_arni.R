library(r4ss)

ctl <- SS_readctl("../01_initial_test/N_05_76_54Amax/swo2025.ctl",
                  datlist="../01_initial_test/N_05_76_54Amax/swo2025.dat")


ctl$Variance_adjustment_list



txt <- readLines("../01_initial_test/N_05_76_54Amax/swo2025.ctl")
# Input variance adjustments
