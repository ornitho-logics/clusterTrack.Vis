
#' Generate a clustered trajectory
#'
#' Creates a continuous-time data frame containing a base trajectory and a set 
#' of spatial clusters generated around selected trajectory points. 
#' Each randmply chosen point becomes the center of a cluster,
#' and points within clusters receive timestamps sampled between the adjacent
#' trajectory timestamps.
#'
#'
#' @param traj_n Integer. Number of base trajectory steps.
#' @param traj_random Logical. Whether the base trajectory is random.
#' @param traj_step_length Numeric. Step length for the generated trajectory.
#' @param n_clusters Integer. Number of cluster centers.
#' @param n_per_cluster Integer. Number of points to draw for each cluster.
#' @param x_range Numeric length-2. Range to rescale x values of the trajectory.
#' @param y_range Numeric length-2. Range to rescale y values of the trajectory.
#' @param sd_range Numeric length-2. Range of standard deviations used to build
#'   cluster covariance matrices.
#'
#' @return A `ctdf` containing both the trajectory and generated cluster points. 
#' A `true_cluster` column marking the ID of each cluster observation.
#'
#' @details
#' All traj_ parameters are passed to trajr::TrajGenerate.
#' For each center, points are drawn from a bivariate normal distribution with a
#' diagonal covariance matrix whose standard deviation is drawn from
#' `sd_range`. Timestamps for cluster points are uniformly sampled between the
#' center's previous and next trajectory timestamps, avoiding exact boundaries.
#'
#' @examples
#' tr = generate_clustered_traj(
#'   traj_n = 200,
#'   n_clusters = 5,
#'   n_per_cluster = 20
#' )
#' tr



  generate_clustered_traj <- function(
    traj_n             = 100,
    traj_random        = FALSE,
    traj_step_length   = 2,
    n_clusters         = 10,
    n_per_cluster      = 10,
    x_range            = c(14, 34),
    y_range            = c(17, -17),
    sd_range           = c(0.2, 0.8)
    ) {

      tr = trajr::TrajGenerate(
        n          = traj_n,
        random     = traj_random,
        stepLength = traj_step_length
      ) |> setDT()

      tr = tr[, .(x, y, time)]
      tr[, time := Sys.time() + 3600000 * time]
      tr[, x := scales::rescale(x, x_range)]
      tr[, y := scales::rescale(y, y_range)]

      tr[, time_prev := shift(time)]
      tr[, time_next := shift(time, type = "lead")]
      tr[, base_id := .I]

      clusters = tr[sample(.N, n_clusters)] |> na.omit()
      clusters[, sd := runif(.N, sd_range[1], sd_range[2])]

      cluster_points = clusters[
        , {
            Sigma = diag(sd^2, 2)
            mvrnorm(
              n = n_per_cluster,
              mu = c(x, y),
              Sigma = Sigma
            ) |> data.table()
          },
        by = base_id
      ]
      setnames(cluster_points, c("true_cluster", "x", "y"))

      sample_between = function(n, time_prev, time_next) {
        d = as.numeric(time_next - time_prev)
        offs = runif(n, 0, d)
        offs = pmin(pmax(offs, 1), d - 1)
        time_prev + lubridate::seconds(offs)
      }

      cluster_points[
        , time := clusters[
            , .(time = sample_between(n_per_cluster, time_prev, time_next)),
            by = base_id
          ]$time
      ]

      setorder(cluster_points, time)
      cluster_points[
        , true_cluster := as.character(true_cluster) |> forcats::fct_inorder() |> as.numeric()
      ]

      out = rbind(
        tr[, .(x, y, time)],
        cluster_points,
        fill = TRUE
      )

      setorder(out, time)
      

      as_ctdf(out, coords = c("x", "y"))

  }