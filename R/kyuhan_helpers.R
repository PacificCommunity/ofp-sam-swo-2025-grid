
# Function to apply likelihood weight adjustments
apply_adjustments <- function(ctlBase, config) {
  if (!is.null(config$multiplier)) {
    # Global multiplier (apply to all likelihood weights)
    ctlBase$Variance_adjustment_list$Value <- 
      ctlBase$Variance_adjustment_list$Value * config$multiplier
    
  } else if (!is.null(config$line_adjustments)) {
    # Line-specific adjustments (apply by line to specific likelihood weights)
    for (i in 1:nrow(config$line_adjustments)) {
      line_num <- config$line_adjustments$Line[i]
      multiplier <- config$line_adjustments$Multiplier[i]
      
      if (line_num <= nrow(ctlBase$Variance_adjustment_list)) {
        ctlBase$Variance_adjustment_list$Value[line_num] <- 
          ctlBase$Variance_adjustment_list$Value[line_num] * multiplier
      }
    }
    
  } else if (!is.null(config$fleet_adjustments)) {
    # Fleet-specific adjustments (apply by Fleet to likelihood weights)
    for (i in 1:nrow(config$fleet_adjustments)) {
      fleet_num <- config$fleet_adjustments$Fleet[i]
      multiplier <- config$fleet_adjustments$Multiplier[i]
      
      fleet_rows <- which(ctlBase$Variance_adjustment_list$Fleet == fleet_num)
      if (length(fleet_rows) > 0) {
        ctlBase$Variance_adjustment_list$Value[fleet_rows] <- 
          ctlBase$Variance_adjustment_list$Value[fleet_rows] * multiplier
      }
    }
  }
  
  return(ctlBase)
}