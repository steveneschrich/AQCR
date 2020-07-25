#' Plot the RNA degradation slopes across probes.
#'
#' This is a graphing function that takes the N (typically 11) average probe expressions
#' and plots the slope of these expressions across the probes. It does this for multiple
#' samples in the dataset.
#'
#' @param QCObject
#' @param transform
#' @param cols
#' @param main
#' @param ymax
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
plot_rna_degradation_slopes<-function(object, transform = "shift.scale", cols = NULL,
                                      main = "RNA degradation plot",
                                      ymax = -1,
                                      ...)
{

  # This is code taken directly from the affy package, affy::plotAffyRNADeg

  # The first part of the code adjust the data so that the values are
  # stacked and scaled.
  if (!is.element(transform, c("shift.scale", "shift.only",
                               "neither")))
    stop("Tranform must be 'shift.scale','shift.only', or 'neither'")
  mns <- get_probe_means_by_number(object)
  if (is.null(cols))
    cols = rep(4, dim(mns)[1])
  ylab = "Mean Intensity"
  height_offset<-ifelse(ymax > nrow(mns), (ymax-nrow(mns))/2,0)

  if (transform == "shift.scale") {
    sds <- get_probe_stderr_by_number(object)
    mn <- mns[, 1]
    mns <- sweep(mns, 1, mn)
    mns <- mns/(sds)
    mns <- sweep(mns, 1, (1:(dim(mns)[1]))+height_offset, "+")
    ylab <- paste(ylab, ": shifted and scaled")
  }
  else if (transform == "shift.only") {
    mn <- mns[, 1]
    mns <- sweep(mns, 1, mn)
    mns <- sweep(mns, 1, (1:(dim(mns)[1]))+height_offset, "+")
    ylab <- paste(ylab, ": shifted")
  }

  # Now that the data is adjusted, set it up for plotting in ggplot.
  # Extract the sample means by probe
  df<-data.frame(t(mns))
  colnames(df)<-get_sample_names(object)
  df$Probe<-1:nrow(df)
  df<-pivot_longer(df, cols=get_sample_names(object), names_to="Sample", values_to="Mean_Intensity")


  # Once the data is adjusted for plotting, plot it.
  large_plot<-(length(get_sample_names(object))>5)
  p<-ggplot(df, aes(x=Probe, y=Mean_Intensity, group=Sample, col=Sample))
  if (large_plot) {
    p <- p + geom_line()
  } else {
    p <- p + geom_line(aes(linetype=Sample))
  }

  p<-p +
    geom_point() +
    #facet_grid(rows=vars(Sample)) #+
    xlab("5' <-----> 3'\nProbe Number") +
    ylab(ylab) +
    scale_x_continuous(breaks=1:11, limits=c(0, dim(mns)[2]+1)) +
    ylim(0 , max(c(ymax, as.vector(mns))) + 1) +
    labs(title=main) +
    theme_bw()
  # Add in a check for really big sample sets, to not print the legend.
  if (large_plot) {
    p<-p + theme(legend.position = "none")
  }

  plist<-list(p)
  names(plist)<-object$name

  plist
}
