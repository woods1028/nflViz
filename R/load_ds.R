#' Dataset Loader
#'
#' @param base_dir
#'
#' @return ds
#' @export
#' @importFrom arrow timestamp float64 int16 int32 int64 string

load_ds <- function(base_dir = "big_data_bowl") {

  tracking_files <- base_dir %>%
    list.files(pattern = "nfl-big-data-bowl",full.names = T) %>%
    purrr::map(~list.files(.x,pattern = "weeks",full.names = T)) %>%
    purrr::map(~list.files(.x,full.names = T))

  schema1 <- arrow::schema(
    time = timestamp(unit = "ns",timezone = "GMT"),
    x = float64(),y = float64(),s = float64(),a = float64(),dis = float64(),o = float64(),dir = float64(),
    event = string(),nflId = int64(),displayName = string(),jerseyNumber = int16(),position = string(),
    frameId = int16(),team = string(),gameId = int32(),playId = int32(),
    playDirection = string(),route = string()
  )

  schema2 <- arrow::schema(
    gameId = int32(),playId = int32(), nflId = int64(), frameId = int16(),
    time = timestamp(unit = "ns"),jerseyNumber = int16(),team = string(),playDirection = string(),
    x = float64(),y = float64(),s = float64(),a = float64(),dis = float64(),o = float64(),
    dir = float64(),event = string()
  )

  schema3 <- arrow::schema(
    gameId = int32(),playId = int32(), nflId = int64(), displayName = string(), frameId = int16(),
    frametype = string(),time = timestamp(unit = "ns"),jerseyNumber = int16(),club = string(),
    playDirection = string(),
    x = float64(),y = float64(),s = float64(),a = float64(),dis = float64(),o = float64(),
    dir = float64(),event = string()
  )

  ds1 <- arrow::open_dataset(tracking_files[[1]],format = "csv",schema = schema1,skip = 1)
  ds2 <- arrow::open_dataset(tracking_files[[2]],format = "csv",schema = schema2,skip = 1)
  ds3 <- arrow::open_dataset(tracking_files[[3]],format = "csv",schema = schema3,skip = 1)

  ds <- ds1 %>%
    dplyr::select(all_of(names(ds2))) %>%
    dplyr::select(-time) %>%
    dplyr::union_all(
      ds2 %>%
        dplyr::select(-time)
    ) %>%
    dplyr::union_all(
      ds3 %>%
        dplyr::rename(team = club) %>%
        dplyr::select(all_of(names(ds2))) %>%
        dplyr::select(-time)
    )

  return(ds)

}
