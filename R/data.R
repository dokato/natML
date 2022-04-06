#' Subset of Hemibrain Kenyon Cell neurons
#'
#' Separated in balances training (300 examples) and test (75 examples) splits.
#'
#' Note that these positions are raw (8nm voxel) locations.
#' @references Scheffer, L.K., Xu, C.S., Januszewski, M., Lu, Z., Takemura, S.Y.,
#' Hayworth, K.J., Huang, G.B., Shinomiya, K., Maitlin-Shepard, J., Berg, S.
#' and Clements, J., 2020. A connectome and analysis of the adult Drosophila
#' central brain. Elife, 9, p.e57443.
#' @name kc_train
#' @rdname sampledata
#' @docType data
#' @examples
#'
#' library(nat)
#' library(natML)
#' plot3d(kc_train)
"kc_train"

#' @name kc_test
#' @rdname sampledata
#' @docType data
"kc_test"
