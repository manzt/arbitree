
<!-- README.md is generated from README.Rmd. Please edit that file -->

# arbitree

<!-- badges: start -->

<!-- badges: end -->

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("manzt/arbitree")
```

## Usage

Arbitree is intended to plugin to the normal Monocle3 workflow. Users
are encouraged to read the
[docs](https://cole-trapnell-lab.github.io/monocle3/docs/trajectories/)
prior to performing trajectory analysis. We’ve wrapped the `learn_graph`
function from Monocle3 with `arbitree_learn_graph`, which exposes
assumed, hard-coded, and hueristic parameters for determinig inital
“landmark cells” for DDRTree. Calling `arbitree_learn_graph` without
any arguments gives the same result as `learn_graph`.

``` r
library(monocle3)
library(arbitree) 

cds <- new_cell_data_set(expression_matrix, cell_metadata, gene_metadata)
##
# perform  normalization, gene selection, dimensionality reduction, and clustering...
##
cds <- learn_graph(cds) # monocle function
cds <- arbitree_learn_graph(cds) # arbitree function (same output)

plot_cells(cds) # plot trajectory
```

As described the
[Monocle 3 paper](https://www.nature.com/articles/s41586-019-0969-x),
“\[Monocle3\] first selects a set of ‘landmark’ cells using by first
running the `kmeans()` clustering algorithm in R with k equal to the
value of the `ncenter` argument, which can be passed to `learn_graph()`
by the user. The landmark cells are then selected by first mapping each
cell to its nearest kmeans point, and then selecting the cell for each
kmeans point with the highest local density. By default, Monocle 3 uses
a data-dependent policy for adjusting ncentre automatically.”

In `arbitree_learn_graph`, we have added `ncenter`, `k_nn`, and
`init_medioids` as parameters.

  - `ncenter`: number of kmeans clusters, which is equal to the number of landmark cells seeding DDRTree because there is one landmark cell per cluster.
  - `k_nn`: number of nearest neighbors used to determine the landmark
    cell with highest local density. In Monocle 3, this parameter is
    hard-coded to 25 and does not scale with the size of the partition.
  - `use_density`: If `TRUE`, landmark cells are selected based on highest density of connectivity to other cells as determined by k-nearest-neighbors clustering. If `FALSE`, landmark cells are selected based on single cell per partition that is closest to the center of the cluster by low dimensional euclidean distance. Arbitree default is `TRUE`. In Monocle 3, `use_density` is not a parameter because `TRUE` behavior is always called. 
  - `input_medioids`: an (`ndim x centroid`) matrix used to seed the
    DDRTree algorithm. Default is `NULL`. If provided, the landmark
    cells used to initialize DDRTree are determined by the cell which is
    nearest in low dimensional euclidean distance to each provided
    centroid, skipping the kmeans and knn.

We created an widget for drawing arbitrary “initial states” over the
reduced dimensions using the `draw_tree` function. Usage is as
follows:

``` r
xy_coords <- draw_tree(cds) # opens shiny app and user draws points on UMAP projection
cds <- arbitree_learn_graph(cds, input_medioids = t(xy_coords))
plot_cells(cds)
```


Created by Trevor Manz & Daniel Weiner

Inspiration from Dr. Peter Kharchenko

December 2019
