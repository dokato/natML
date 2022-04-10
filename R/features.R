# This script curates selection of the most popular
# scalar features of NAT neurons.

#' Get a neuron spine length
#'
#' @param n (nat) neuron
#' @return numeric with spine length
#' @import nat
#' @family neuron_scalars
#' @export
get_spine_length <- function(n) {
  nat:::total_cable(spine(n))
}

#' Get cable length
#'
#' @param n (nat) neuron
#' @return numeric with total cable length
#' @import nat
#' @family neuron_scalars
#' @export
get_cable_length <- function(n) {
  nat:::total_cable(n)
}

#' Get soma size
#'
#' @param n (nat) neuron
#' @return numeric with soma size
#' @family neuron_scalars
#' @export
get_soma_size <- function(n) {
  n$d[n$StartPoint,]$W
}

#' Get average radius
#'
#' @param n (nat) neuron
#' @return numeric with soma size
#' @family neuron_scalars
#' @export
get_radius <- function(n) {
  mean(n$d$W)
}

#' Get average spine radius
#'
#' @param n (nat) neuron
#' @return numeric with soma size
#' @import nat
#' @family neuron_scalars
#' @export
get_spine_radius <- function(n) {
  mean(spine(n)$d$W)
}

#' Get number of branchpoints
#'
#' @param n (nat) neuron
#' @return numeric with soma size
#' @import nat
#' @family neuron_scalars
#' @export
get_n_branchpoints <- function(n) {
  sum(branchpoints(n))
}
