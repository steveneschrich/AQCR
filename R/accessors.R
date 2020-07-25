#' Get the list of existing packages used by AQCR.
#'
#' @return
#' @export
#'
#' @examples
get_package_lists<-function() {
  tibble::tribble(
    ~Package, ~URL,
    "simpleaffy","https://www.bioconductor.org/packages/release/bioc/vignettes/simpleaffy/inst/doc/simpleAffy.pdf",
    "yaqc","https://www.bioconductor.org/packages/release/bioc/vignettes/yaqcaffy/inst/doc/yaqcaffy.pdf",
    "affy","http://bioconductor.org/packages/release/bioc/vignettes/affy/inst/doc/affy.pdf",
    "qcmetrics","https://bioconductor.org/packages/release/bioc/manuals/qcmetrics/man/qcmetrics.pdf"
  )
}



#' Title
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
sample_names<-function(object) {
  sampleNames(object$affybatch)
}

#' Title
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
get_sample_names<-function(object) {
  sampleNames(object$affybatch)
}
#' Title
#'
#' @param object
#' @param i
#'
#' @return
#' @export
#'
#' @examples
get_sample_name<-function(object, i) {
  get_sample_names(object)[i]
}
#' Extract ratios for GAPDH, Beta Actin genes.
#'
#' @param object The composite QC object
#'
#' @return Ratios for all samples in the object.
#' @export
#'
#' @examples
get_control_gene_ratios<-function(object) {
  ratios(object$simpleaffy)
}

#' Return the names of existing ratios within the dataset.
#'
#' @param object The QC dataset
#'
#' @return A character vector of names of ratios in the QC object.
#' @export
#'
#' @examples
get_control_gene_ratio_names<-function(object) {
  colnames(ratios(object$simpleaffy))
}

#' Title
#'
#' @param object
#' @param name
#'
#' @return
#' @export
#'
#' @examples
get_control_gene_ratio_byname<-function(object, name) {
  ratios(object$simpleaffy)[,name]
}

#' Title
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
get_control_gene_ratios_actin35<-function(object) {
  ratios(object$simpleaffy)[,"actin3/actin5"]
}

#' Title
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
get_control_gene_ratios_actin3M<-function(object) {
  ratios(object$simpleaffy)[,"actin3/actinM"]
}

#' Title
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
get_control_gene_ratios_gapdh35<-function(object) {
  ratios(object$simpleaffy)[,"gapdh3/gapdh5"]
}

#' Title
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
get_control_gene_ratios_gapdh3M<-function(object) {
  ratios(object$simpleaffy)[,"gapdh3/gapdhM"]
}



#' Get slope values from RNA Degradation.
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
get_slope<-function(object) {
  m<-object$affyrnadeg$slope
  names(m)<-object$affyrnadeg$sample.names

  m
}

#' Title
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
get_scaling_factors<-function(object) {
  s<-sfs(object$simpleaffy)
  names(s)<-get_sample_names(object)

  s
}

#' Title
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
get_probe_means_by_number<-function(object) {
  object$affyrnadeg$means.by.number
}
#' Title
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
get_probe_stderr_by_number<-function(object) {
  object$affyrnadeg$ses
}


#' Title
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
get_array_type<-function(object) {
  qc.get.array()
}
