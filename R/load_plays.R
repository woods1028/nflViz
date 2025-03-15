#' Play Loader
#'
#' @param base_dir
#'
#' @return plays_clean
#' @export
#' @importFrom purrr map reduce
#' @importFrom dplyr mutate bind_rows select
#' @importFrom stringr str_detect str_extract

load_plays <- function(base_dir = "big_data_bowl") {

  ds <- load_ds()

  plays <- base_dir %>%
    list.files(pattern = "nfl-big-data-bowl",full.names = T) %>%
    map(~list.files(.x,pattern = "plays",full.names = T)) %>%
    map(readr::read_csv)

  games <- base_dir %>%
    list.files(pattern = "nfl-big-data-bowl",full.names = T) %>%
    map(~list.files(.x,pattern = "games",full.names = T)) %>%
    map(readr::read_csv)

  games <- games[[1]] %>%
    mutate(season = 2018) %>%
    bind_rows(
      games[[2]],
      games[[3]] %>%
        select(-homeFinalScore,visitorFinalScore)
    )

  coverages_week1_2018 <- readr::read_csv(
    file.path(base_dir,"nfl-big-data-bowl-2021/coverages_week1.csv")
  )

  ## fix the missing defensiveTeam field from 2018 data
  plays[[1]] <- games %>%
    tidyr::pivot_longer(cols = c(homeTeamAbbr,visitorTeamAbbr)) %>%
    group_by(gameId) %>%
    summarise(across(c(value),list)) %>%
    list(
      plays[[1]],
      .
    ) %>%
    reduce(left_join,by = "gameId") %>%
    mutate(defensiveTeam = pmap(list(value,possessionTeam),function(x,y) x[x != y])) %>%
    tidyr::unnest(cols = defensiveTeam) %>%
    list(
      coverages_week1_2018 %>%
        mutate(across(coverage,~sub(" \\w+$","",.x)),
               across(coverage,~sub(" ","-",.x))) %>%
        rename(pff_passCoverage = coverage)
    ) %>%
    reduce(inner_join,by = c("gameId","playId"))

  ## fix coverages from 2022 data
  plays[[3]] <- plays[[3]] %>%
    filter(isDropback == T) %>%
    mutate(
      across(pff_passCoverage,~sub("Cover ","Cover-",.x)),
      across(pff_passCoverage,~ifelse(str_detect(.x,"Cover"),str_extract(.x,"Cover-\\d"),.x))
    )

  plays_clean <- plays %>%
    map(~select(.x,gameId,playId,playDescription,quarter,down,yardsToGo,possessionTeam,defensiveTeam,pff_passCoverage,
                preSnapHomeScore,preSnapVisitorScore,gameClock)) %>%
    list(c(2018,2021,2022)) %>%
    pmap(function(x,y) mutate(x,season = y)) %>%
    bind_rows

  return(plays_clean)

}
