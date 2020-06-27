AQCR stands for the Affymetrix Quality Control Reporter. Briefly, many different QC packages exist for Affymetrix quality control but this one focuses on the quality of these reports in terms of breadth, content and presentation. Rather than reinventing every test, this package aims to consolidate the available techniques and wrap them in convenient (and attractive) output.

# Overall Goal
The goal of the AQCR project is simple: to provide an extensive set of QC routines for Affymetrix GeneChips that is easy to use, attractive to use, and comprehensive. 

# General Overview of Package
There are two main components of the package: single sample and dataset-level analysis. In both cases, QC does not exist in a vacuum; it should be considered with respect to other examples. That is the common theme in this package.

## Single Sample QC
There are many occasions in which a single sample must be invidividually inspected for quality. Questions that arise include "Are there visual artifacts on the chip?" or "Does the RNA appear degraded?". As an example of one of the challenges that I frequently face, what is an acceptable scaling factor for a chip? I have some sense of this based on the fact that I have seen many chips, but why not provide the statistics about a single array compared to a reference population? This would allow me (or others) to have more information when making a decision regarding a sample.

## Dataset QC
The notion of dataset QC is all-encompassing and this package is not intended to deal with this all-encompassing notion. For instance, clinical variables or clinical variables with respect to the array are not dealt with here. Rather, dataset QC is focused on assessing the population of arrays and comparing this to a reference population (an existing dataset). Whereas single sample QC would provide insight into a particular parameter, the question for dataset QC is how does the range of parameters compare to other datasets? Are the scaling factors too variable? Do the density plots of signal differ more than expected? If an appropriate reference exists, is covariance among genes maintained? 