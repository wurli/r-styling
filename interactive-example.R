# lintr::lint() / styler::style_selection() ------------------------------------

# NB: This code doesn't run - it's just for demonstrating styling problems

# load packages
library(tidyverse);library(lubridate)

data_raw <- read_csv("some_file.csv")

# This is a very very very very very very very very very very very very very long comment
data_clean <- data_raw %>%
 mutate(Amount = Amount / sum(Amount),
    # Combine date parts into single column
Date = make_date(Year, Month, Day)
  )%>%
   filter(
    # other years aren't relevant to analysis
    year(Date)==2020,
    Amount>0.1
  )

ggplot2(data_clean, aes(Date, Amount))+geom_line()

# iris %>%
#   mutate(
#     Sepal.Area = Sepal.Width * Sepal.Length
#   )

# Use in .lintr file to increase line length
# and allow commented out code
# linters: linters_with_defaults(
#     line_length_linter(120),
#     commented_code_linter = NULL
#   )


# [Ctrl + I]: Correct indentation ----------------------------------------------

iris %>%
  mutate(
    Sepal.Area = Sepal.Length * Sepal.Width,
    Petal.Area = Petal.Length * Petal.Width
  )



# [Alt + Ctrl + Shift + M]: Rename in scope ------------------------------------

x <- 1234

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}



# [Ctrl + Shift + F]: Find/replace in project ----------------------------------
# Try searching for `dplyr::`



# [Ctrl + Shift + /]: Reflow comment -------------------------------------------

# This is a comment which spans a few lines. It could probably span less... But
# rewriting it could be a lot of work. This is solved by using Ctrl + Shift + /
# to 'reflow' the comment.



# [Alt + -] and [Ctrl + Shift + M]: Insert -> and %>% --------------------------

my_var <- data %>%
  do_something()



# [Alt + Ctrl + Shift + R]: Insert template documentation ----------------------

#' Clean numbers
#'
#' @param x A character vector
#' @param remove Characters to remove
#'
#' @return A numeric vector
#' @export
#'
#' @examples
#'
#' clean_numbers(c("??1.5", "??300", "$200", "not a number"))
clean_numbers <- function(x, remove = c("??", "$", "%")) {

  stopifnot(is.character(x))

  for (char in remove) {
    x <- stringr::str_remove_all(x, stringr::fixed(char))
  }

  as.numeric(x)
}





# [Alt + click/drag]: Multiline cursor -----------------------------------------

tibble::tribble(
  ~category, ~value,
   "A", 1,
   "A", 2,
   "B", 3,
   "C", 4
)


