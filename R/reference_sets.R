#' Download and extract reference datasets.
#'
#' This function calls convenience functions that download
#' and extract the different datasets so that the reference
#' QC metrics can be run against them.
#'
#' @return Nothing, but directories with reference datasets are created.
#' @export
#'
#' @examples
create_references<-function() {
  create_nci60()
  create_maqc()

  invisible(NULL)
}

#' Generate QCMetrics for the reference sets.
#'
#' Starting with a list of name=path pairs, generate
#' QC metrics associated with dataset name, where the
#' CEL files are in path.
#'
#' @param reference_datasets A paired list of name=path reference datasets.
#'
#' @return A list of reference QC metrics (see generateQCMetrics).
#' @export
#'
#' @examples
generate_reference_QCMetrics<-function(reference_datasets) {

  refs<-list()
  for (i in 1:length(reference_datasets)) {
    d<-names(reference_datasets)[i]
    message(sprintf("Processing reference dataset %s.", d))
    refs[[d]]<-generateQCMetrics(d, reference_datasets[[i]])
  }

  refs
}



#' Retrieve reference table of QC values (accessed by f)
#'
#' The goal of this function is to assemble reference values from the
#' reference set (ref) into a table for summary. The reference set is
#' a list of QC objects. The accessor function (f) can be used for
#' each dataset to extract the necessary values. It is expected that the
#' actual values extracted are unique to each sample in the reference set
#' so summarization occurs.
#'
#' The results are returned as a list of lists since one of the return values is
#' a ecdf function on the actual values.
#'
summarize_referenceset_qcmetric<-function(target, refs, f) {
  sn<-get_sample_names(target)

  rs<-lapply(refs, function(d) {
    summarize_reference_qcmetric(target, d, f)
  })

  rs
}


#' Title
#'
#' @param target
#' @param refs
#' @param f
#' @param metric_name
#'
#' @return
#' @export
#'
#' @examples
create_qcmetric_referenceset_summary<-function(target, refs, f) {
  metric_name<-names(f)[1]

  summaries<-create_qcmetric_referenceset_summaries(target, refs, f)
  rs<-purrr::map_dfc(summaries, function(ref) {
    dplyr::mutate(ref, Sample=NULL, !!metric_name := NULL)
  })
  rs<-dplyr::mutate(rs,
                    Sample=get_sample_names(target),
                    !!metric_name := f[[1]](target)
  )
  rs<-dplyr::select(rs, Sample, !!metric_name, dplyr::everything())

  rs
}

#' Retrieve a list of summaries by reference set as a list.
#'
#' See create_qcmetric_referenceset_summary for a function that
#' merges these.
#'
#' @param target
#' @param refs
#' @param f
#'
#' @return
#' @export
#'
#' @examples
create_qcmetric_referenceset_summaries<-function(target, refs, f) {
  metric_name<-names(f)[[1]]
  rs<-purrr::map(refs, function(ref) {
    create_qcmetric_reference_summary(target_data, ref, f)

  })

  rs
}

#' Get the qcmetric information for a metric in a dataset.
#'
#' Given a specific dataset (d), extract the metric (function f)
#' and return various summarizations of this metric. Note the
#' metric can consist of multiple columns of data so the
#' list may consist of a number of entries.
#'
#' @param d
#' @param f
#'
#' @return
#' @export
#'
#' @examples
summarize_reference_qcmetric<-function(target, ref, f) {
  requireNamespace("rlang")
  requireNamespace("dplyr")
  requireNamespace("tibble")

  ref_vals<-f[[1]](ref)
  target_vals<-f[[1]](target)
  n<-length(target_vals)
  metric_name <- names(f)[1]

  df<-tibble::tibble(
    Sample=get_sample_names(target),
    !!metric_name := target_vals,
    outlier=calculate_outlier(target_vals, ref_vals),
    mean=rep(mean(ref_vals), n),
    sd=rep(sd(ref_vals), n),
    range_min=rep(min(ref_vals), n),
    range_max=rep(max(ref_vals), n),
    quantile=ecdf(ref_vals)(target_vals)
  )

  df
}

#' Create a formatted summary of a qcmetric from a reference dataset.
#'
#' This provides a tibble showing a reference dataset summarized by
#' a specific qcmetric (extracted by function f). The target dataset
#' and reference dataset are needed for these calculations. Note this
#' only formats the output of summarize_reference_qcmetric() for printing
#' purposes.
#'
#' @param target
#' @param ref
#' @param f
#'
#' @return
#' @export
#'
#' @examples
create_qcmetric_reference_summary<-function(target, ref, f) {
  df<-summarize_reference_qcmetric(target, ref, f)
  fdf<-dplyr::mutate(tibble::as_tibble(df),
                     !!sprintf("%s status", ref$name) := ifelse(outlier, "Outside Range", "Within Range"),
                     !!sprintf("%s quantile", ref$name) := sprintf("%2.1f", 100*quantile),
                     !!sprintf("%s mean(sd)", ref$name) := sprintf("%.3f (%.3f)", mean, sd),
                     !!sprintf("%s range", ref$name) := sprintf("%.3f-%.3f", range_min, range_max),
                     value=NULL,
                     mean=NULL,
                     sd=NULL,
                     range_min=NULL,
                     range_max=NULL,
                     quantile=NULL,
                     outlier=NULL)
  fdf

}

#' Calculates if target_vals have outliers (TRUE/FALSE) based on ref_vals
#' distribution.
#'
#' This function uses the approach of the boxplot, namely an outlier is
#' determined if the value is beyond 1.5*IQR of the distribution.
#'
#' Here we change it up a little bit. The distribution is defined by ref_vals,
#' not by the target_vals data. Therefore the comparison is looking if the
#' target values are inside a reasonable range of expected values for the
#' ref_vals.
#'
#' @param target_vals
#' @param ref_vals
#'
#' @return
#' @export
#'
#' @examples
calculate_outlier<-function(target_vals, ref_vals) {
  stats<-stats::fivenum(ref_vals)
  iqr<-diff(stats[c(2,4)])
  whisker<- 1.5 * iqr

  target_vals < stats[2] - whisker | target_vals > stats[4] + whisker
}




