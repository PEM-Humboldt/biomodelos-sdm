
give.msg.time <- function(time.1) {
  time.2 <- Sys.time()

  # Time spent

  alltime <- round(as.numeric(difftime(time.2, time.1, units = "hours")), 2)

  t.min <- round((alltime - trunc(alltime)) * 60)

  msg <- paste("Completed in ", trunc(alltime), "hours", t.min, "mins\n")

  return(msg)
}