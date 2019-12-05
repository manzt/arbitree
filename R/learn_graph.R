#' Learn principal graph from the reduced dimension space using reversed graph
#' embedding
#'
#' @description Monocle3 aims to learn how cells transition through a
#' biological program of gene expression changes in an experiment. Each cell
#' can be viewed as a point in a high-dimensional space, where each dimension
#' describes the expression of a different gene. Identifying the program of
#' gene expression changes is equivalent to learning a \emph{trajectory} that
#' the cells follow through this space. However, the more dimensions there are
#' in the analysis, the harder the trajectory is to learn. Fortunately, many
#' genes typically co-vary with one another, and so the dimensionality of the
#' data can be reduced with a wide variety of different algorithms. Monocle3
#' provides two different algorithms for dimensionality reduction via
#' \code{reduce_dimension} (UMAP and tSNE). Both take a cell_data_set object
#' and a number of dimensions allowed for the reduced space. You can also
#' provide a model formula indicating some variables (e.g. batch ID or other
#' technical factors) to "subtract" from the data so it doesn't contribute to
#' the trajectory. The function \code{learn_graph} is the fourth step in the
#' trajectory building process after \code{preprocess_cds},
#' \code{reduce_dimensions}, and \code{cluster_cells}. After
#' \code{learn_graph}, \code{order_cells} is typically called.
#'
#' @section Optional \code{learn_graph_control} parameters:
#' \describe{
#'   \item{euclidean_distance_ratio:}{The maximal ratio between the euclidean
#'   distance of two tip nodes in the spanning tree and the maximum distance
#'   between any connecting points on the spanning tree allowed to be connected
#'   during the loop closure procedure. Default is 1.}
#'   \item{geodesic_distance_ratio:}{The minimal ratio between the geodesic
#'   distance of two tip nodes in the spanning tree and the length of the
#'   diameter path on the spanning tree allowed to be connected during the loop
#'   closure procedure. (Both euclidean_distance_ratio and
#'   geodesic_distance_ratio need to be satisfied to introduce the edge for
#'   loop closure). Default is 1/3.}
#'   \item{minimal_branch_len:}{The minimal length of the diameter path for a
#'   branch to be preserved during graph pruning procedure. Default is 10.}
#'   \item{orthogonal_proj_tip:}{ Whether to perform orthogonal projection for
#'   cells corresponding to the tip principal points. Default is FALSE.}
#'   \item{prune_graph:}{Whether or not to perform an additional round of graph
#'   pruning to remove small insignificant branches. Default is TRUE.}
#'   \item{scale:}{}
#'   \item{ncenter:}{}
#'   \item{maxiter:}{}
#'   \item{eps:}{}
#'   \item{L1.gamma:}{}
#'   \item{L1.sigma:}{}
#' }
#'
#' @param cds the cell_data_set upon which to perform this operation
#' @param k the number of neighbors for knn
#' @param k_louvain the number of clusters during close_loop
#' @param use_partition logical parameter that determines whether to use
#'   partitions calculated during \code{cluster_cells} and therefore to learn
#'   disjoint graph in each partition. When \code{use_partition = FALSE}, a
#'   single graph is learned across all partitions. Default is TRUE.
#' @param close_loop logical parameter that determines whether or not to
#'   perform an additional run of loop closing after estimating the principal
#'   graphs to identify potential loop structure in the data space. Default is
#'   TRUE.
#' @param learn_graph_control NULL or a list of control parameters to be
#'   passed to the reversed graph embedding function. Default is NULL. A list
#'   of potential control parameters is provided in details.
#' @param verbose Whether to emit verbose output during graph learning.
#' @return an updated cell_data_set object
#' @export
learn_graph_ <- function(cds, medioids=NULL, k=NULL, k_louvain = 25, use_partition = TRUE, close_loop = TRUE, learn_graph_control = NULL, verbose = FALSE) {
  reduction_method <- "UMAP"
  
  # Create defaults
  if (!is.null(learn_graph_control)) {
    assertthat::assert_that(methods::is(learn_graph_control, "list"))
    assertthat::assert_that(all(
      names(learn_graph_control) 
      %in% 
      c(
        "euclidean_distance_ratio",
        "geodesic_distance_ratio",
        "minimal_branch_len",
        "orthogonal_proj_tip",
        "prune_graph",
        "scale",
        "ncenter",
        "maxiter",
        "eps",
        "L1.gamma",
        "L1.sigma"
      )),
      msg = "Unknown variable in learn_graph_control"
    )
  }
  euclidean_distance_ratio <- ifelse(is.null(learn_graph_control$euclidean_distance_ratio), 1, learn_graph_control$euclidean_distance_ratio)
  geodesic_distance_ratio <- ifelse(is.null(learn_graph_control$geodesic_distance_ratio), 1/3, learn_graph_control$geodesic_distance_ratio)
  minimal_branch_len <- ifelse(is.null(learn_graph_control$minimal_branch_len), 10, learn_graph_control$minimal_branch_len)
  orthogonal_proj_tip <- ifelse(is.null(learn_graph_control$orthogonal_proj_tip), FALSE, learn_graph_control$orthogonal_proj_tip)
  prune_graph <- ifelse(is.null(learn_graph_control$prune_graph), TRUE, learn_graph_control$prune_graph)
  ncenter <- learn_graph_control$ncenter
  scale <- ifelse(is.null(learn_graph_control$scale), FALSE, learn_graph_control$scale)
  maxiter <- ifelse(is.null(learn_graph_control$maxiter), 10, learn_graph_control$maxiter)
  eps <- ifelse(is.null(learn_graph_control$eps), 1e-5, learn_graph_control$eps)
  L1.gamma <- ifelse(is.null(learn_graph_control$L1.gamma), 0.5, learn_graph_control$L1.gamma)
  L1.sigma <- ifelse(is.null(learn_graph_control$L1.sigma), 0.01, learn_graph_control$L1.sigma)
  
  # Check arguments
  assertthat::assert_that(methods::is(cds, "cell_data_set"))
  assertthat::assert_that(is.logical(use_partition))
  assertthat::assert_that(is.logical(close_loop))
  assertthat::assert_that(is.logical(verbose))
  assertthat::assert_that(is.logical(orthogonal_proj_tip))
  assertthat::assert_that(is.logical(prune_graph))
  assertthat::assert_that(is.logical(scale))
  assertthat::assert_that(is.numeric(euclidean_distance_ratio))
  assertthat::assert_that(is.numeric(geodesic_distance_ratio))
  assertthat::assert_that(is.numeric(minimal_branch_len))
  if(!is.null(ncenter)) assertthat::assert_that(assertthat::is.count(ncenter))
  assertthat::assert_that(assertthat::is.count(maxiter))
  assertthat::assert_that(is.numeric(eps))
  assertthat::assert_that(is.numeric(L1.sigma))
  assertthat::assert_that(is.numeric(L1.sigma))
  assertthat::assert_that(!is.null(reducedDims(cds)[[reduction_method]]),
                          msg = paste("No dimensionality reduction for",
                                      reduction_method, "calculated.",
                                      "Please run reduce_dimensions with",
                                      "reduction_method =", reduction_method,
                                      "and cluster_cells before running",
                                      "learn_graph."))
  assertthat::assert_that(!is.null(cds@clusters[[reduction_method]]),
                          msg = paste("No cell clusters for",
                                      reduction_method, "calculated.",
                                      "Please run cluster_cells with",
                                      "reduction_method =", reduction_method,
                                      "before running learn_graph."))
  
  if (use_partition) {
    partition_list <- cds@clusters[[reduction_method]]$partitions
  } else {
    partition_list <- rep(1, nrow(colData(cds)))
  }
  
  multi_tree_DDRTree_res <- multi_component_RGE(
    cds, 
    medioids = medioids,
    k_init = k,
    k_louvain = k_louvain,
    scale = scale,
    reduction_method = reduction_method,
    partition_list = partition_list,
    irlba_pca_res = reducedDims(cds)[[reduction_method]],
    max_components = max_components,
    ncenter = ncenter,
    maxiter = maxiter,
    eps = eps,
    L1.gamma = L1.gamma,
    L1.sigma = L1.sigma,
    close_loop = close_loop,
    euclidean_distance_ratio = euclidean_distance_ratio,
    geodesic_distance_ratio = geodesic_distance_ratio,
    prune_graph = prune_graph,
    minimal_branch_len = minimal_branch_len,
    verbose = verbose
  )
  
  rge_res_W <- multi_tree_DDRTree_res$ddrtree_res_W
  rge_res_Z <- multi_tree_DDRTree_res$ddrtree_res_Z
  rge_res_Y <- multi_tree_DDRTree_res$ddrtree_res_Y
  cds <- multi_tree_DDRTree_res$cds
  dp_mst <- multi_tree_DDRTree_res$dp_mst
  
  
  principal_graph(cds)[[reduction_method]] <- dp_mst
  cds@principal_graph_aux[[reduction_method]]$dp_mst <- rge_res_Y
  
  cds <- project2MST(cds, project_point_to_line_segment, orthogonal_proj_tip, verbose, reduction_method, rge_res_Y)
  
  cds
}