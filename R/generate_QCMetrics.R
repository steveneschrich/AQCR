#' Generate QC Metrics for a given dataset.
#'
#' There are a number of different packages and routines that generate
#' various QC metrics for an Affy array. This function calls each of them
#' to generate a list of QC metrics that can be used to graph or otherwise
#' output information on.
#'
#' @param name The name of the dataset.
#' @param path Path to the celfiles for the dataset.
#'
#' @return A list of objects representing different QC approaches.
#' @export
#'
#' @examples
generate_QCMetrics<-function(name, path) {
  requireNamespace("affy")
  requireNamespace("simpleaffy")
  requireNamespace("yaqcaffy")

  message(sprintf("%s: Loading CEL files as affybatch.", name))
  abatch<-affy::read.affybatch(list.celfiles(path, full.names=TRUE))
  message(sprintf("%s: Generate Affy RNA Degradation result.", name))
  affyrnadeg<-affy::AffyRNAdeg(abatch)
  message(sprintf("%s: Generate simpleaffy result.", name))
  simpleaffy<-simpleaffy::qc(abatch)
  message(sprintf("%s: Generate YAQC result.", name))
  yaqc<-yaqcaffy::yaqc(abatch, verbose=TRUE)

  list(affybatch = abatch,
       affyrnadeg = affyrnadeg,
       simpleaffy = simpleaffy,
       yaqc = yaqc,
       name = name)

}
