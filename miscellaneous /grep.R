filelist <- list.files(recursive = TRUE)

lapply(
  filelist,
  function(x){
    file <- readLines(x)
    lines <- grep("hex", file)
    paste(lines, file[lines])
  }
)