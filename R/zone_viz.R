#' Zone Builder
#'
#' @param x_breaks numeric
#' @param y_breaks numeric
#'
#' @return zone_grid
#' @export
#'
#' @importFrom dplyr tibble

zone_builder <- function(x_breaks, y_breaks) {

  x_labels <- c("left third","middle","right third")

  if (length(y_breaks) == 6) {

    y_labels <- c("real deep", "deep", "deep hooks", "shallow hooks", "blitz")

  } else {

    y_labels <- c("real deep", "deep", "hooks", "blitz")

  }

  xzones = x_labels %>% map(~rep(.x,length(y_labels))) %>% unlist

  yzones = rep(y_labels,length(x_labels))

  deep_zones <- sum(str_detect(y_labels,"deep"))
  hook_zones <- sum(str_detect(y_labels,"hook"))

  zones = c(
    rep("third_left",deep_zones + hook_zones - 1), "flat_left", "blitz",
    rep("middle",deep_zones), rep("hooks",hook_zones), "blitz",
    rep("third_right",deep_zones + hook_zones - 1),"flat_right","blitz"
  )

  zone_grid <- tibble(xzone = xzones, yzone = yzones,zone = zones)

  return(zone_grid)

}

#' Zone Tiles
#'
#' @param x_breaks numeric
#' @param y_breaks numeric
#' @param zone_grid data.frame
#' @param plot_width_limits numeric
#' @param plot_height_limits numeric
#'
#' @return zone_tiles_plot
#' @export
#'
#' @import dplyr
#' @import ggplot2

zone_tile <- function(x_breaks, y_breaks, zone_grid, plot_width_limits, plot_height_limits) {

  zone_tiles <- crossing(
    x_edge = x_breaks,
    y_edge = y_breaks
  ) %>%
    mutate(
      across(
        x_edge,
        ~ifelse(. < plot_width_limits[1],plot_width_limits[1],
                ifelse(. > plot_width_limits[2],plot_width_limits[2],.))
      ),
      across(
        y_edge,
        ~ifelse(. < plot_height_limits[1],plot_height_limits[1],
                ifelse(. > plot_height_limits[2],plot_height_limits[2],.))
      )
    ) %>%
    distinct %>%
    group_by(x_edge) %>%
    mutate(y_lead = lead(y_edge)) %>%
    group_by(y_edge) %>%
    mutate(x_lead = lead(x_edge)) %>%
    ungroup %>%
    mutate(
      x = rowMeans(select(.,x_edge,x_lead)),
      y = rowMeans(select(.,y_edge,y_lead)),
      w = abs(x_edge - x)*2,
      h = abs(y_edge - y)*2
    ) %>%
    filter(!is.na(x),!is.na(y)) %>%
    mutate(
      xzone = cut(x,x_breaks,x_labels),
      yzone = cut(y,y_breaks,y_labels)
    ) %>%
    left_join(zone_grid,by = c("xzone","yzone")) %>%
    mutate(sym_zone = sub("_.+$","",zone))

  zone_tiles_plot <- geom_tile(
    data = zone_tiles,
    mapping = aes(x = x, y = y, width = w,height = h,fill = sym_zone),
    alpha = .7
  )

  return(zone_tiles_plot)

}

#' Zone Viz
#'
#' @param ds dataset
#' @param x_breaks numeric
#' @param y_breaks numeric
#'
#' @export
#'
#' @import dplyr
#' @import ggplot2

zone_viz <- function(ds, x_breaks, y_breaks) {

  coverages_to_model = c(
    "Cover-0",
    "Cover-1",
    "Cover-3",
    "Cover-2",
    "Quarters",
    "Cover-6"
  )

  snap_events <- c("ball_snap","autoevent_ballsnap","snap_direct")

  events_to_clip <- c(
    'pass_forward','autoevent_passforward','qb_sack','run','pass_outcome_incomplete','pass_arrived',
    'autoevent_passinterrupted','qb_strip_sack','pass_tipped','fumble','pass_outcome_caught','handoff',
    'pass_shovel','qb_spike','pass_outcome_interception', 'lateral', 'tackle',
    'fumble_defense_recovered', 'fumble_offense_recovered', 'qb_slide',
    'pass_outcome_touchdown', 'dropped_pass', 'safety'
  )

  play_crop <- ds %>%
    distinct(gameId,playId,frameId,event) %>%
    group_by(gameId,playId) %>%
    mutate(
      snap_event = event %in% snap_events,
      clip_event = event %in% events_to_clip
    ) %>%
    list(
      filter(.,snap_event == T),
      filter(.,clip_event == T)
    ) %>%
    .[-1] %>%
    map(function(x) {

      group_by(x,gameId,playId) %>%
        summarise(across(frameId,min),.groups = "drop") %>%
        collect

    }) %>%
    list(c("snap","clip")) %>%
    pmap(function(x,y) mutate(x,type = y)) %>%
    bind_rows %>%
    pivot_wider(id_cols = c(gameId,playId),names_from = type,values_from = frameId)

  spots <- ds %>%
    filter(team == "football") %>%
    mutate(across(c(playId,gameId,frameId),as.integer)) %>%
    inner_join(
      play_crop %>%
        rename(frameId = snap) %>%
        mutate(across(frameId,as.integer)),
      by = c("gameId","playId","frameId")
    ) %>%
    select(gameId,playId,spot_x = x,spot_y = y) %>%
    collect

  mean_positions <- ds %>%
    left_join(
      play_crop,
      by = c("gameId","playId")
    ) %>%
    filter(frameId >= snap & (frameId <= clip | is.na(clip))) %>%
    list(
      plays_clean %>%
        mutate(across(c(gameId,playId),as.integer)) %>%
        select(gameId,playId,defensiveTeam,yardsToGo)
    ) %>%
    reduce(inner_join,by = c("gameId","playId")) %>%
    filter(team == defensiveTeam) %>%
    left_join(
      spots,
      by = c("gameId","playId")
    ) %>%
    mutate(
      flip_direction = ifelse(playDirection == "right","counterclockwise","clockwise"),
      newx = ifelse(flip_direction == "counterclockwise", y, -y + 53.33),
      newy = ifelse(flip_direction == "counterclockwise", -x, x),
      newo = ifelse(flip_direction == "counterclockwise",-(o +90), -(o - 90)),
      newspot_y = ifelse(flip_direction == "counterclockwise", -spot_x,spot_x),
      newspot_x = ifelse(flip_direction == "counterclockwise",spot_y, -spot_y + 53.33),
      line_to_gain = newspot_y - yardsToGo
    ) %>%
    mutate(across(
      c(newy,newspot_y,line_to_gain),
      ~ifelse(flip_direction == "counterclockwise",.x + 110,.x - 10)
    )) %>%
    group_by(gameId,playId,nflId,jerseyNumber) %>%
    list(
      distinct(.,newspot_y,newspot_x,line_to_gain) %>%
        ungroup,
      summarise(
        .,
        across(c(newx,newy),mean),
        .groups = "drop"
      )
    ) %>%
    .[-1] %>%
    reduce(left_join,by = c("gameId","playId","nflId","jerseyNumber")) %>%
    collect

  depth_by_play <- mean_positions %>%
    inner_join(
      plays_clean %>%
        filter(pff_passCoverage %in% coverages_to_model) %>%
        select(gameId,playId,pff_passCoverage),
      by = c("gameId","playId")
    ) %>%
    mutate(
      depth = newy - newspot_y,
      width = newx - newspot_x,
      first_down = line_to_gain - newspot_y
    ) %>%
    group_by(gameId,playId) %>%
    mutate(
      width_rk = data.table::frankv(width,order = -1,ties.method = "min"),
      side = ifelse(width < 0,"L","R"),
      depth_rk = data.table::frankv(depth,order = 1,ties.method = "min")
    ) %>%
    ungroup

  depth_by_level <- depth_by_play %>%
    group_by(gameId,playId) %>%
    mutate(depth_rk = data.table::frankv(depth,order = 1,ties.method = "min")) %>%
    ungroup %>%
    filter(depth_rk <= 7) %>%
    mutate(level = ifelse(depth < -15,2,1)) %>%
    group_by(gameId,playId,level) %>%
    mutate(width_rk = data.table::frankv(width,order = 1,ties.method = "min")) %>%
    ungroup %>%
    group_by(pff_passCoverage,level,width_rk) %>%
    summarise(
      count = n(),
      across(c(depth,width),mean),
      .groups = "drop"
    )

  zone_grid <- zone_builder(x_breaks, y_breaks)

  depth_by_zone <- depth_by_play %>%
    mutate(
      xzone = cut(width,breaks = x_breaks,x_labels),
      yzone = cut(depth,breaks = y_breaks,y_labels)
    ) %>%
    left_join(
      zone_grid,
      by = c("xzone","yzone")
    ) %>%
    mutate(sym_zone = sub("_.+$","",zone)) %>%
    group_by(gameId,playId) %>%
    mutate(depth_rk = data.table::frankv(depth,order = 1,ties.method = "min")) %>%
    ungroup %>%
    filter(depth_rk <= 7) %>%
    group_by(gameId,playId,zone,sym_zone) %>%
    mutate(width_rk = data.table::frankv(width,order = 1,ties.method = "min")) %>%
    ungroup %>%
    group_by(pff_passCoverage,zone,sym_zone,width_rk) %>%
    summarise(
      count = n(),
      across(c(depth,width),mean),
      .groups = "drop"
    ) %>%
    ungroup %>%
    group_by(pff_passCoverage) %>%
    mutate(rk = data.table::frankv(count,order = -1,ties.method = "min")) %>%
    ungroup %>%
    filter(rk <= 7) %>%
    group_by(pff_passCoverage) %>%
    slice(1:7) %>%
    mutate(pct = count/sum(count)) %>%
    ungroup %>%
    mutate(across(pff_passCoverage,~factor(.,levels = coverages_to_model)))

  plot_center <- c(0,-7.5)
  plot_width <- 40
  plot_height <- 25
  plot_width_limits <- plot_center[1] + c(-plot_width/2, plot_width/2)
  plot_height_limits <- plot_center[2] +c(-plot_height/2, plot_height/2)

  zone_tiles_plot <- zone_tile(x_breaks, y_breaks, zone_grid, plot_width_limits, plot_height_limits)

  depth_by_zone %>%
    ggplot(mapping = aes(x = width,y = depth))+
    zone_tiles_plot +
    geom_hline(yintercept = 0,lty = 3)+
    geom_vline(xintercept = 0,lty = 3)+
    geom_vline(xintercept = -3.081667,lty = 2)+
    geom_vline(xintercept = 3.081667,lty = 2)+
    geom_point(
      mapping = aes(size = pct,group = zone),
      show.legend = F,color = "dodgerblue4",fill = "cornflowerblue",shape = 21, stroke = 2
    ) +
    facet_wrap(vars(pff_passCoverage))+
    scale_size_continuous(range = c(7,10))+
    scale_fill_manual(values = c(
      "blitz" = "indianred1",
      "flat" = "palegreen3",
      "hooks" = "lightblue2",
      "third" = "lightgoldenrod1",
      "middle" = "plum"
    )) +
    labs(fill = "",y = "Depth",x = "Width")+
    theme(
      strip.text = element_text(size = 14,color = "white",face = "bold"),
      strip.background = element_rect(fill = "dodgerblue4"),
      legend.text = element_text(size = 12),
      panel.background = element_rect(fill = "white"),
      panel.grid = element_line(color = "gray90"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      axis.title.y = element_text(angle = 0,vjust = .5)
    )

}
