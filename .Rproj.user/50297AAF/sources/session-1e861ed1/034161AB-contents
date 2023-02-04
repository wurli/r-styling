#' Make missing values explicit
#'
#' Some providers may have no recorded starts for some months. This function
#' makes sure such cases are explicit in the data, i.e. by adding rows
#' with zero starts. This is useful for some modelling functions to work
#' properly.
#'
#' @param starts Starts data
#' @param fill_cols Columns to pad with zeros
#' @param id_col A column which identifies providers
#' @param time_col The time column to expand - this is done by month
#' @param time_min,time_max Values between these dates will be filled. If left
#'   `NULL`, these will be calculated as the first/last available dates for
#'   each provider
#' @param ... Additional columns to calculate when creating new rows
#'
#' @return A dataframe
#' @export
fill_gaps <- function(starts,
                      fill_cols = "Starts",
                      id_col = "UKPRN",
                      time_col = "Start_Date",
                      time_min = NULL,
                      time_max = NULL,
                      ...) {

  mutated_cols <- names(enexprs(...))

  # 1. Get other columns which can be used as identifiers
  other_identifiers <- starts |>
    select(-all_of(c(fill_cols, time_col, mutated_cols))) |>
    summarise(
      .by = UKPRN,
      across(everything(), n_distinct)
    ) |>
    summarise(across(-UKPRN, ~ all(.x == 1L))) |>
    pivot_longer(
      everything(),
      names_to = "Column_Name",
      values_to = "Is_Unique_To_Provider"
    )

  not_unique <- other_identifiers |>
    filter(!Is_Unique_To_Provider) |>
    pull(Column_Name)

  if (length(not_unique) > 0) {
    cli_warn(c(
      "Can't fill values for some columns",
      i = "Check {.field {not_unique}}",
      i = "These columns will be padded with {.val {NA}}"
    ))

    other_identifiers <- other_identifiers |>
      filter(Is_Unique_To_Provider)
  }

  other_identifiers <- other_identifiers |>
    pull(Column_Name)

  # 2. Get implicit gaps as explicit
  new_rows <- starts |>
    reframe(
      .by = all_of(c(id_col, other_identifiers)),
      across(
        all_of(time_col),
        ~ seq.Date(
          time_min %||% min(.),
          time_max %||% max(.),
          by = "1 month"
        )
      ),
      across(all_of(fill_cols), ~ 0L),
      ...
    )

  # 3. Insert new rows
  starts |>
    rows_insert(
      new_rows,
      by = c(id_col, time_col, mutated_cols),
      conflict = "ignore"
    ) |>

    # 4. Arrange - not necessary but nice for QOL
    arrange(.data[[id_col]], .data[[time_col]])

}
