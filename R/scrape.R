
#' Metadata in documents
#'
#' @param wd A file directory, defaults to current working directory
#' @param filetype The type of files to scrape, defaults to pdf only
#' @param cutoff The proportion of documents with metadata field to return, defaults to 1 (all fields)
#' @return A dataframe with the metadata for all documents within the
#'    file path directory
#' @export
#' @examples
#' scrape()

scrape <- function(wd = NULL,
                   filetype = NULL,
                   verbose = F,
                   cutoff = 1){

  if(is.null(filetype)){filetype <- "pdf"}


  if(is.null(wd)){wd <- getwd()}

  if(dir.exists(wd) == F){
    stop("Not a valid working directory")
  }

  ifelse(filetype == "all",
         files <- list.files(path = wd, pattern = "*.pdf$", recursive = TRUE),
         files <- list.files(path = wd, recursive = TRUE, pattern = "\\.pdf$|\\.doc$"))

  files <- paste(wd, files, sep = "/")

  d <- data.frame(NA)

  if(verbose == T){pb = utils::txtProgressBar(min = 0, max = length(files), initial = 0) }

  for(i in 1:length(files)){di <- tryCatch(exifr::read_exif(files[i]), error = function(e) e)
  d <- dplyr::bind_rows(dplyr::mutate_all(d, as.character), dplyr::mutate_all(di, as.character))
  if(verbose == T){utils::setTxtProgressBar(pb,i)}}

  d <- d[,-1]
  d <- d[-1,]
  d$file <- files
  na_count <- 1 - (sapply(d, function(y) sum(length(which(is.na(y))))) / nrow(d))
  d <- d[,order(na_count)]
  d <- d[,which(na_count>=cutoff)]
  d
}
