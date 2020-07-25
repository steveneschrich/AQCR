# From the GEO website
# https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE32474
# Comparison between cell lines from 9 different cancer tissue of origin types
# (Breast, Central Nervous System, Colon, Leukemia, Melanoma, Non-Small Cell Lung,
# Ovarian, Prostate, Renal) from NCI-60 panel.

#' Create the NCI60 qc objects and save them in the
#' appropriate directory.
#'
#' @return invisibly, the qc object.
#' @export
#'
#' @examples
#'
#' create_maqc()
create_nci60<-function() {
  download_nci60_dataset()
  extract_nci60_dataset()

}

#' Download the NCI60 GEO dataset for Affymetrix (HG-U133Plus array).
#'
#' @param dir - The target directory for storing the CEL files (default data/maqc)
#' @param url - The URL to get the data from (default is GEO).
#'
#' @return - Nothing
#' @export
#'
#' @examples
#' download_nci60_dataset(dir="/tmp/nci60")
#'
#' This downloads the nci60 dataset in /tmp/nci60.
#'
download_nci60_dataset<-function(dir="data/nci60",
                                url="https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE32474&format=file") {
  if ( !dir.exists(dir)) {
    dir.create(dir, recursive=TRUE)
  }
  tarfile<-file.path(dir, "nci60.tar")
  if ( !file.exists(tarfile)) {
    download.file(url=url, destfile=tarfile)
  }
}


#' Extract nci60 dataset from tar file.
#'
#' @param tarfile - The tarfile with NCI60 data (data/nci60/nci60.tar)
#' @param dir - The directory to put CEL files (data/nci60)
#'
#' @return - nothing
#' @export
#'
#' @examples
#'
#' extract_nci60_dataset(tarfile="data/nci60/nci60.tar", dir="data/nci60")
#'
extract_nci60_dataset<-function(tarfile="data/nci60/nci60.tar", dir="data/nci60") {
  stopifnot(file.exists(tarfile))
  if ( !dir.exists(dir)) {
    dir.create(dir, recursive=TRUE)
  }

  untar(tarfile, exdir=dir)
}

