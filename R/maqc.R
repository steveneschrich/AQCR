# From the GEO website
# https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE5350
# Expression data from two distinct reference RNA samples (A and B) in four titration
# pools were generated at multiple test sites using a variety of microarray-based and
# alternative technology platforms. Sample A = Stratagene Universal Human Reference
# RNA (UHRR, Catalog #740000), Sample B = Ambion Human Brain Reference RNA (HBRR,
# Catalog #6050), Sample C = Samples A and B mixed at 75%:25% ratio (A:B); and
# Sample D = Samples A and B mixed at 25%:75% ratio (A:B). In general, each microarray
# platform was tested at three sites and each sample was tested in five replicates at
# each test site. Samples (hybridizations) were named according to the following convention:
# Platform_Testsite_SampleRelicate. For example, AFX_2_B1 represents the hybridization
# (array) from platform AFX processed by test site 2 for the first replicate of sample B.
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3272078/



#' Create the MAQC qc objects and save them in the
#' appropriate directory.
#'
#' @return invisibly, the qc object.
#' @export
#'
#' @examples
#'
#' create_maqc()
create_maqc<-function() {
  download_maqc_dataset()
  extract_maqc_dataset()

}


#' Download the MAQC GEO dataset for Affymetrix (HG-U133Plus array).
#'
#' @param dir - The target directory for storing the CEL files (default data/maqc)
#' @param url - The URL to get the data from (default is GEO).
#'
#' @return - Nothing
#' @export
#'
#' @examples
#' download_maqc_dataset(dir="/tmp/maqc")
#'
#' This downloads the maqc dataset in /tmp/maqc.
#'
download_maqc_dataset<-function(dir="data/maqc",
                                url="https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE5350&format=file&file=GSE5350%5FMAQC%5FAFX%5F123456%5F120CELs%2Ezip") {
  if ( !dir.exists(dir)) {
    dir.create(dir, recursive=TRUE)
  }
  zipfile<-file.path(dir, "maqc.zip")
  if ( !file.exists(zipfile)) {
    download.file(url=url, destfile=zipfile)
  }
}


#' Extract maqc dataset from zip file.
#'
#' @param zipfile - The zipfile with MAQC data (data/maqc/maqc.zip)
#' @param dir - The directory to put CEL files (data/maqc)
#'
#' @return - nothing
#' @export
#'
#' @examples
#'
#' extract_maqc_dataset(zipfile="data/maqc/maqc.zip", dir="data/maqc")
#'
extract_maqc_dataset<-function(zipfile="data/maqc/maqc.zip", dir="data/maqc") {
  stopifnot(file.exists(zipfile))
  if ( !dir.exists(dir)) {
    dir.create(dir, recursive=TRUE)
  }

  unzip(zipfile, exdir=dir)
}




