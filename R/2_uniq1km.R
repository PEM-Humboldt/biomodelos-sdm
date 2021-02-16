do.uniq1km <- function(occ., col.lon, col.lat, sp.col, sp.name) {
  
  dropUniq <- spThin::thin(
    loc.data = occ., lat.col = col.lat, long.col = col.lon, spec.col = sp.col, thin.par = 1,
    reps = 20, verbose = TRUE, locs.thinned.list.return = TRUE, write.files = FALSE
    # thin.par parameter should be setting according to species natural history
  )

  dropsmall <- dropUniq[[1]]

  dropsmall[, sp.col] <- rep(sp.name, nrow(dropsmall))

  dropsmall <- dropsmall[, c(3, 1, 2)]

  names(dropsmall) <- c(sp.col, col.lon, col.lat)

  return(dropsmall)
}
