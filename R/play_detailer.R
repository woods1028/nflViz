#' Title
#'
#' @param plays_clean
#'
#' @export
#'
#' @importFrom dplyr select mutate rowwise ungroup

play_detailer <- function(plays_clean, games) {

  plays_clean %>%
    select(-season) %>%
    left_join(
      games,
      by = "gameId"
    ) %>%
    mutate(across(
      c(possessionTeam,defensiveTeam),
      list(score = ~ifelse(.x == homeTeamAbbr,preSnapHomeScore,preSnapVisitorScore))
    )) %>%
    mutate(
      down_distance = paste(scales::ordinal(down),"&",yardsToGo),
      scoreboard = paste(possessionTeam_score,defensiveTeam_score,sep = " - "),
      sitch = paste(scales::ordinal(quarter),"Quarter,",str_extract(gameClock,"\\d+:\\d+")),
      location = ifelse(possessionTeam == homeTeamAbbr,"v","@"),
      across(location,~paste0(possessionTeam," ",.x," ",defensiveTeam," ",gameDate))
    ) %>%
    rowwise %>%
    mutate(caption = paste(location,sitch,scoreboard,down_distance,sep = "\n")) %>%
    ungroup

}
