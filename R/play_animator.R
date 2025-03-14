#' Play Animator
#'
#' @param ds arrow dataset
#' @param game_id integer
#' @param play_id integer
#' @param play_details data.frame
#' @param color_scheme data.frame
#'
#' @export
#'
#' @import dplyr
#' @import ggplot2

play_animator <- function(ds, game_id, play_id, play_details, color_scheme) {

  play_frames <- ds %>%
    filter(playId == play_id,gameId == game_id) %>%
    collect %>%
    list(
      filter(.,event == "ball_snap") %>%
        mutate(across(frameId,~.x - 1)) %>%
        select(frameId,gameId,playId)
    ) %>%
    magrittr::inset2(
      length(.) + 1,
      semi_join(
        .[[1]] %>%
          filter(team == "football"),
        .[[2]],
        by = c("gameId","playId","frameId")
      ) %>%
        select(spot = x)
    ) %>%
    .[-2] %>%
    bind_cols %>%
    mutate_at(vars(jerseyNumber),~ifelse(team == "football",NA,as.integer(.))) %>%
    mutate_at(vars(o),~tidyr::replace_na(.,0)) %>%
    group_by(nflId) %>%
    mutate(frame_level = factor(frameId)) %>%
    ungroup

  play_caption <- play_details %>%
    filter(gameId == game_id,playId == play_id) %>%
    pull(caption)

  play_df <- play_frames %>%
    left_join(
      color_scheme,
      by = c("gameId","playId","team")
    ) %>%
    left_join(
      play_details,
      by = c("gameId","playId")
    ) %>%
    left_join(
      players %>%
        select(nflId,position,season),
      by = c("nflId","season")
    ) %>%
    mutate(skill = ifelse(position %in% c("WR","FB","TE","RB"),"black",NA)) %>%
    mutate(
      flip_direction = ifelse(playDirection == "right","counterclockwise","clockwise"),
      newx = ifelse(flip_direction == "counterclockwise", y, -y + 53.33),
      newy = ifelse(flip_direction == "counterclockwise", -x, x),
      newo = ifelse(flip_direction == "counterclockwise",-(o +90), -(o - 90)),
      newspot = ifelse(flip_direction == "counterclockwise", -spot,spot),
      line_to_gain = newspot - yardsToGo,
      across(color,~ifelse(team == "football","#8d4d37",.x)),
      number_color = ifelse(color == "#FFFFFF","black","white"),
      shape = ifelse(team == "football",18,19)
    )

  yard_line_range <- play_df$newy %>%
    range

  goal_lines <- list()

  for (spot in c(10,110,-10,-110)) {

    if (between(spot,yard_line_range[1],yard_line_range[2])) {

      goal_line <- geom_hline(yintercept = spot,color = "red2")

    } else {

      goal_line <- list()

    }

    goal_lines <- goal_lines %>%
      magrittr::inset2(
        length(.) + 1,
        goal_line
      )

  }

  route_lines <- play_df %>%
    bind_cols(
      filter(.,event == "ball_snap") %>%
        distinct(frameId) %>%
        rename(ball_snap = frameId)
    ) %>%
    filter(skill == "black",frameId >= ball_snap) %>%
    select(-frame_level)

  field_lines <- list(
    geom_vline(xintercept = 29.71667,lty = 2),
    geom_vline(xintercept = 23.5833,lty = 2),
    geom_vline(xintercept = 12,lty = 3),
    geom_vline(xintercept = 6,lty = 3),
    geom_vline(xintercept = 53.33 - 12,lty = 3),
    geom_vline(xintercept = 53.33 - 6,lty = 3),
    geom_vline(xintercept = 0,lty = 1),
    geom_vline(xintercept = 53.33,lty = 1)
  )

  final_plot <- play_df %>%
    {
      ggplot(.,mapping = aes(x = newx, y = newy))+
        field_lines +
        geom_hline(yintercept = unique(.$newspot),color = "navy")+
        geom_hline(yintercept = unique(.$line_to_gain),color = "yellow2",lwd = 2)+
        goal_lines +
        geom_path(data = route_lines, aes(x = newx, y = newy,group = nflId), color = "black")+
        geom_point(aes(color = color,shape = shape),size = 7,show.legend = F)+
        scale_color_identity()+
        scale_shape_identity()+
        geom_text(aes(label = jerseyNumber,angle = newo,color = number_color))+
        scale_y_continuous(labels = yard_lines) +
        labs(x = "",y = "",caption = play_caption)+
        ggtitle(label = unique(.$pff_passCoverage))+
        coord_fixed(ratio = 1) +
        theme(
          plot.caption = element_text(size = 12,vjust = .5,margin = margin(t = 40, b = 0)),
          plot.title = element_text(size = 16,hjust = .5),
          axis.text.x = element_blank()
        ) +
        gganimate::transition_states(frame_level,transition_length = 2,state_length = 1)
    }

  gganimate::animate(final_plot,fps = 10,renderer = gganimate::ffmpeg_renderer())

}

#' Yard Line Helper Function
#'
#' @param yard_line
#'
#' @return ylabel
#' @export
#'
yard_lines <- function(yard_line) {

  ylabel = ifelse(yard_line > 0,yard_line,-yard_line)

  ylabel = ifelse(ylabel > 60, 110 - ylabel, ylabel - 10)

  return(ylabel)

}

#' Color Scheme Maker
#'
#' @return color_scheme
#' @export
#'
#' @import dplyr

color_generator <- function() {

  color_grid <- readr::read_csv("color_grid.csv")

  color_scheme <- plays_clean %>%
    left_join(
      color_grid %>%
        mutate(across(c(team1,team2),~ifelse(. == "LAR","LA",.))) %>%
        select(team1,team2,possessionColor = color1,defensiveColor = color2),
      by = c("possessionTeam" = "team1","defensiveTeam" = "team2")
    ) %>%
    list(
      tidyr::pivot_longer(.,cols = matches("Team"),names_to = "side",values_to = "team") %>%
        select(gameId,playId,side,team) %>%
        mutate(across(side,~sub("Team","",.))),
      tidyr::pivot_longer(.,cols = matches("color"),names_to = "side",values_to = "color") %>%
        select(gameId,playId,side,color) %>%
        mutate(across(side,~sub("Color","",.)))
    ) %>%
    .[-1] %>%
    reduce(left_join,by = c("gameId","playId","side")) %>%
    select(-side)

  return(color_scheme)

}
