read_spu <- function(pjnz_path) {

  spufile <- get_filename_from_extension("SPU", pjnz_path)
  ## readLines
  spu <- utils::read.csv(unz(pjnz_path, spufile), header=FALSE)

  n.resamp <- as.numeric(strsplit(as.character(spu[1,1]), " ")[[1]][3])
  break.rows <- which(spu[,1] == "==")
  n.years <- break.rows[2] - break.rows[1] - 2
  count <- sapply(strsplit(as.character(spu[break.rows[-1]-(n.years+1),1]), " "), function(x) as.numeric(x[2]))

  years <- as.numeric(as.character(spu[break.rows[1]-n.years:1,1]))

  incid <- sapply(break.rows[-1], function(idx) spu[idx-n.years:1,3])[,rep(1:length(count), count)]/100
  prev <- sapply(break.rows[-1], function(idx) spu[idx-n.years:1,2])[,rep(1:length(count), count)]/100

  rownames(incid) <- years
  rownames(prev) <- years

  return(list("incid"=incid, "prev"=prev))
}
