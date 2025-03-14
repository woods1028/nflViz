#' Load Players
#'
#' @param base_dir
#'
#' @return players
#' @export
#'
#' @import dplyr
#' @importFrom purrr map pmap

load_players <- function(base_dir = "big_data_bowl") {

  players <- base_dir %>%
    list.files(pattern = "nfl-big-data-bowl",full.names = T) %>%
    map(~list.files(.x,pattern = "players",full.names = T)) %>%
    map(~readr::read_csv(.x) %>%
          setNames(c("nflId","height","weight","birthDate","collegeName","position","displayName"))) %>%
    list(c(2018,2021,2022)) %>%
    pmap(function(x,y) mutate(x,season = y)) %>%
    bind_rows

  return(players)

}
