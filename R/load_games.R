#' Game Loader
#'
#' @param base_dir
#'
#' @return games
#' @export
#'
#' @importFrom dplyr mutate bind_rows select
#' @importFrom purrr map
#' @importFrom readr read_csv

load_games <- function(base_dir = "big_data_bowl") {

  games <- base_dir %>%
    list.files(pattern = "nfl-big-data-bowl",full.names = T) %>%
    map(~list.files(.x,pattern = "games",full.names = T)) %>%
    map(read_csv)

  games <- games[[1]] %>%
    mutate(season = 2018) %>%
    bind_rows(
      games[[2]],
      games[[3]] %>%
        select(-homeFinalScore,visitorFinalScore)
    )

  return(games)

}
