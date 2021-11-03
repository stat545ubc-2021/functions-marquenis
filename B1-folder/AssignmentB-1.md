In the following assignment, I created and described a function inspired
by my personal research. I am an ecological entomologist, interested in
studying how mosquitoes will react to global warming and climate change.

One trait we can study in mosquitoes is body size, so I made a function
which can quickly create a boxplot and summary statistics of the body
size of insects (or any measured numeric variable) from different
experimental growth treatments.

The function I made below is not meant to provide publishable figures
and does not use any statistical analyses to determine significant
relationships. It is meant to be used at the beginning of data analysis,
to gain a general understanding of how the data looks.

First we will begin by loading packages

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.5     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.0.2     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(testthat)
```

    ## 
    ## Attaching package: 'testthat'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     matches

    ## The following object is masked from 'package:purrr':
    ## 
    ##     is_null

    ## The following objects are masked from 'package:readr':
    ## 
    ##     edition_get, local_edition

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     matches

Below is the function.

``` r
#' Quick boxplot and summary statistics 
#'
#' 'box_and_stats()' returns a boxplot and summary statistics (max, min, mean, 
#'    median, standard deviation) for a dataset which contains categorical 
#'    treatment groups and a measured numeric variable. This function was made 
#'    to assess the body size of insects (or any measured numeric variable) from 
#'    different experimental growth treatments. 
#'
#' @param data a tidy data set. The data must be 'tidy' for this function to 
#'    work, so each column must be a variable, each row must be a unique 
#'    observation, and each cell must contain a single value.
#' @param x a column which contains different categorical treatments, and can be 
#'    characters or factors. These are the treatments groups the measured 
#'    variable will be divided into for analyses. The presence of NA values is
#'    not permitted.
#' @param y a column which contains the measured numeric variable. This is 
#'    dependent variable the summary statistics will be calculated from. The 
#'    presence of NA values is permitted, however they will be removed for analyses. 
#' @return This function will return two items. The first item being a boxplot 
#'    displaying the data from the numeric measured variable grouped by the 
#'    identified treatments. The second item is a tibble containing 6 columns: 
#'    1 column containing the different treatments, and 5 columns for the 
#'    minimum, maximum, mean, median, and standard deviation for the measured 
#'    variable from each treatment. 

box_and_stats <- function(data, x, y) {
  
   x_data_check <- dplyr::summarise(data,
                                   is_character_x = is.character({{x}}) | is.factor({{x}}),
                                   class_x = class({{x}}))
  if(!x_data_check$is_character_x) {
    stop("Selected x-column is not character or factor, column is:", x_data_check$class_x)}
   
   y_data_check <- dplyr::summarise(data,
                                   is_numeric_y = is.numeric({{y}}),
                                   class_y = class({{y}}))
  if(!y_data_check$is_numeric_y) {
    stop("Selected y-column is not numeric, column is:", y_data_check$class_y)}
   
  data_to_analyze <- data %>%
    dplyr::mutate(fun_treatment = as.factor({{x}})) %>%
    dplyr::mutate(fun_bodysize = {{y}}) %>%
    dplyr::filter(fun_treatment != "NA") %>%
    dplyr::filter(fun_bodysize != "NA")
  
  quick_boxplot <- data_to_analyze %>%
    ggplot(mapping= aes(x=fun_treatment, y=fun_bodysize))+ 
    geom_boxplot(mapping =aes(x=fun_treatment,y=fun_bodysize),width=0.5)+
    geom_jitter(mapping =aes(x=fun_treatment, y=fun_bodysize), width = 0.1, alpha = 0.6)+
    theme_minimal()+
    ylab("Body size measure") +
    xlab("Treatment")
    
  quick_stats <- data_to_analyze %>%
    dplyr::group_by(fun_treatment) %>%
    dplyr::summarize(min = min(fun_bodysize),
                     max = max(fun_bodysize),
                     mean = mean(fun_bodysize),
                     median = median(fun_bodysize),
                     SD = sd(fun_bodysize))
  
  quick_results <- list(quick_boxplot, quick_stats)
                     
  return(quick_results)
  
}
```

The following code chunk will demonstrate this function used on the
built-in R data set ‘iris’. The categorical treatment variable here will
be Species, and the measured numeric variable will be Sepal.Width. As
you can see, function ignores all other variables in the data set,
focusing only on Species and Sepal.Width, returning a boxplot and brief
summary statistics.

With this quick function, I am quickly able to see that the species
‘setosa’ is larger than the species ‘versicolor’. This newly found
information may prompt me to further investigate the differences between
these two species, running statistical tests to determine if there are
any significance differences between species.

``` r
box_and_stats(iris, Species, Sepal.Width)
```

    ## [[1]]

![](AssignmentB-1_files/figure-markdown_github/iris%20example-1.png)

    ## 
    ## [[2]]
    ## # A tibble: 3 × 6
    ##   fun_treatment   min   max  mean median    SD
    ##   <fct>         <dbl> <dbl> <dbl>  <dbl> <dbl>
    ## 1 setosa          2.3   4.4  3.43    3.4 0.379
    ## 2 versicolor      2     3.4  2.77    2.8 0.314
    ## 3 virginica       2.2   3.8  2.97    3   0.322

Here I used the built-in ‘mtcars’ data set to show that the inputs for
the function are strict. While this data set does not contain body size
measured from different treatments, the function is flexible and can
work for any data as long as the parameters are followed. This data set
can work because it is tidy data, however the input for “x” was neither
a character nor a factor. Even though “cylinders” in cars can be
considered categorical, in this data set R has it as a character class.

``` r
box_and_stats(mtcars, cyl, wt)
```

    ## Error in box_and_stats(mtcars, cyl, wt): Selected x-column is not character or factor, column is:numeric

If we would still like to use this function, we would have to convert
the cyl column into a character or factor, and rerun the function. Below
we see that the function now works. While some data sets may use numbers
as categorical variables, this added step on the users behalf ensures
that this function is used correctly by forcing the user to consciously
convert numeric categorical data into characters or factors.

``` r
mtcars_new <- mtcars %>%
  mutate_at(vars(cyl), factor)

box_and_stats(mtcars_new, cyl, wt)
```

    ## [[1]]

![](AssignmentB-1_files/figure-markdown_github/mtcars%20example%202-1.png)

    ## 
    ## [[2]]
    ## # A tibble: 3 × 6
    ##   fun_treatment   min   max  mean median    SD
    ##   <fct>         <dbl> <dbl> <dbl>  <dbl> <dbl>
    ## 1 4              1.51  3.19  2.29   2.2  0.570
    ## 2 6              2.62  3.46  3.12   3.22 0.356
    ## 3 8              3.17  5.42  4.00   3.76 0.759

Below I am testing my function.

``` r
test_that("Error message with incorrect x input type", {
  expect_error(box_and_stats(mtcars, cyl, wt))
})
```

    ## Test passed 😸

``` r
test_that("Error message with incorrect y input type", {
  expect_error(box_and_stats(iris, species, "hello"))
})
```

    ## Test passed 🥳

``` r
test_that("Returns the correct outputs", {
  subject <- box_and_stats(iris, Species, Sepal.Width)
  expect_s3_class(subject[[1]], "ggplot")
  expect_s3_class(subject[[2]], "data.frame")
})
```

    ## Test passed 🥇

``` r
test_that("Returns correct output types", {
  expect_type(box_and_stats(iris, Species, Sepal.Width), "list")
})
```

    ## Test passed 🎊
