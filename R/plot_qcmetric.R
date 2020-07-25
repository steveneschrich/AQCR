#' Plot a QCMetric with corresponding reference set(s).
#'
#' From a target dataset, with respect to one or more reference datasets, plot
#' a single QC Metric. The QC metric is defined as a single element list whose name
#' is the Metric and whose value is a function that accesses the QC Metric from an
#' object.
#'
#' @param target
#' @param refs
#' @param f
#'
#' @return A list of reference datasets. Each reference dataset consists of a list of plots for each sample.
#' @export
#'
#' @examples
plot_qcmetric_with_referenceset<-function(target, refs, f) {
  stopifnot(length(f)==1)

  p<-purrr::map(refs, function(d) {
    plot_qcmetric_reference(target, d, f)
  })

  p
}

#' Plot a QCMetric for a target dataset, including a single reference set.
#'
#' This function assumes that target consists of multiple samples. The plot
#' shows a single sample relative to a reference population (ref). Therefore
#' a list of such plots, one for each sample, is generated here and returned.
#'
#' @param target The dataset of target samples.
#' @param ref The reference dataset to plot against.
#' @param f The accessor function for the QCMetric of interst. It should be a single item, named list.
#'
#' @return A list of ggplot objects, one per name, that can be plotted.
#' @export
#'
#' @examples
plot_qcmetric_reference<-function(target, ref, f) {
  target_qcmetrics<-f[[1]](target)

  qcmetric_plots<-purrr::lmap(target_qcmetrics, function(metric) {
    plot_qcmetric_reference_single(qcmetric_name = names(f)[1],
                                   target_qcmetric = metric[[1]],
                                   sample_name = names(metric)[1],
                                   ref_qcmetrics = f[[1]](ref),
                                   ref_name = ref$name)
  })

  qcmetric_plots
}


#' Plot a single sample's QC Metric against a reference dataset.
#'
#' This routine produces a plot showing the distribution of a metric in
#' a reference dataset (as both a density plot and jittered points). At
#' the bottom is the target metric value (for a single sample).
#'
#' @param qcmetric_name - The name of the metric being plotted.
#' @param target_qcmetric - A list consisting of a single named value representing a sample metric. The name is the sample name.
#' @param sample_name - The name of the sample with the target qcmetric.
#' @param ref_qcmetrics - A list of sample metrics representing a reference dataset.
#' @param ref_name - The name of the ref dataset.
#'
#' @return A ggplot object representing this plot.
#' @export
#'
#' @examples
plot_qcmetric_reference_single<-function(qcmetric_name,
                                         target_qcmetric,
                                         sample_name,
                                         ref_qcmetrics,
                                         ref_name,
                                         show_xlab=FALSE) {

  # This plots only a single point (for now).
  stopifnot(length(target_qcmetric) == 1)

  # Height of the graph, so we can use scatter points up to that height.
  maxy<-max(density(ref_qcmetrics)$y)
  df<-data.frame(ref=ref_qcmetrics, baseline=rep(maxy/2, length(ref_qcmetrics)))

  # Create the figure in a list, so we can name the list with the sample name.
  p<-ggplot(df, aes(ref)) +
    geom_jitter(aes(y=baseline), width=0, height=maxy/2) +
    geom_density() +
    theme_bw() +
    geom_point(aes(x=target_qcmetric, y=-0.3), color="red", size=6, shape=17) +
    geom_text(aes(x=target_qcmetric, y=-0.6), color="red", label=sample_name) +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    coord_fixed(ratio=1/4) +
    ylim(c(-1, maxy)) +
    xlab("")

  if ( show_xlab ) {
    p<-p+xlab(sprintf("%s (%s)", qcmetric_name, ref_name))
  }
  plist<-list(p)
  names(plist)<-sample_name

  plist
}

