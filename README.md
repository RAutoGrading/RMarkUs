
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RMarkUs: Automated Grading of R Markdown Assignments with MarkUs

<!-- badges: start -->

[![R-CMD-check](https://github.com/siqi-zheng/RMarkUs/actions/workflows/check-release.yaml/badge.svg)](https://github.com/siqi-zheng/RMarkUs/actions/workflows/check-release.yaml)

<!-- badges: end -->

**RMarkUs** is an R package designed to streamline the grading process
of R Markdown assignments (.qmd & .rmd) by integrating with the
[**MarkUs**](https://github.com/MarkUsProject/Markus) online grading
platform. Built on top of the popular
[`testthat`](https://testthat.r-lib.org/) package, **RMarkUs** provides
a robust and flexible framework for automatic grading, enabling
educators to efficiently evaluate students’ code, analyses, and reports
directly within their R Markdown submissions.

**Key Features:**

- **Seamless Integration with MarkUs**: Automatically fetch, grade, and
  upload grades for R Markdown assignments submitted to
  [**MarkUs**](https://github.com/MarkUsProject/Markus), eliminating
  manual grading tasks and saving time for educators.
- **Test-Driven Grading**: Leverages `testthat` to define grading
  criteria as test cases, allowing for customizable, reproducible, and
  transparent grading.
- **Scalable and Efficient**: Designed to handle large courses with
  hundreds of students, **RMarkUs** automates the entire grading
  pipeline, ensuring consistency and fairness across all submissions.

**Who Should Use RMarkUs?**

**RMarkUs** is a project initiated by educators at the Department of
Statistical Sciences in the University of Toronto. **RMarkUs** is ideal
for educators, teaching assistants, and course administrators who manage
large programming courses and want to automate the grading of
assignments submitted through
[**MarkUs**](https://github.com/MarkUsProject/Markus).

## [Getting started](https://htmlpreview.github.io/?https://github.com/siqi-zheng/RMarkUs/blob/development-siqi/vignettes/RMarkUs.html)

If you are just getting started with **RMarkUs** we recommend starting
with the
[**tutorial**](https://htmlpreview.github.io/?https://github.com/siqi-zheng/RMarkUs/blob/development-siqi/vignettes/RMarkUs.html).

### Installation

You can install the development version of RMarkUs from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("siqi-zheng/RMarkUs@development-siqi", dependencies = TRUE, build_vignettes = TRUE)
```

A local version of tutorial can be installed by specifying
`build_vignettes = TRUE`.

and then

``` r
browseVignettes("RMarkUs")
```

You can select HTML on the page to read the tutorial.

### Example

This is a basic example which shows you how to solve a common problem:

``` r
library(RMarkUs)
## basic example code
```

## Resources

- [testthat](https://testthat.r-lib.org/)
- [MarkUs](https://github.com/MarkUsProject/Markus)
