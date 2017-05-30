# datadogr

[![Travis-CI Build Status](https://travis-ci.org/yutannihilation/K9.svg?branch=master)](https://travis-ci.org/yutannihilation/K9)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/yutannihilation/K9?branch=master&svg=true)](https://ci.appveyor.com/project/yutannihilation/K9)

## About

datadogr is a simple package for querying data from [Datadog](https://www.datadoghq.com/).

## Installation

datadogr is not on CRAN yet. You can install with `devtools::install_github()`

``` r
devtools::install_github("yutannihilation/K9")
```

## Usage

### Authentication

You must have an API key for Datadog to use APIs. Follow the instruction on the official document: http://docs.datadoghq.com/api/#authentication

datadogr package uses `DATADOG_API_KEY` environmental variable for the API key. You can set this by:

* defining the environmental variables in `.Renviron` and restart R session.
* inputting on the popup of `k9_auth()` interactively.

### Get List of Metrics

``` r
k9_list_metrics()
```

### Get Metrics Values

A query can be specified with `query` argument using a query string:

```r
k9_get_metrics(
  query = "system.cpu.idle{role:db,environment:test}by{host,region}",
  from  = Sys.time() - 3600,
  to    = Sys.time()
)
```

Or, the same thing can be done with `metric`, `scope` and `by` separetely:

```r
k9_get_metrics(
  metric = "system.cpu.idle",
  scope = list(role = "db", environment = "test"),
  by    = c("host", "region"),
  from  = Sys.time() - 3600,
  to    = Sys.time()
)
```

The result will look like this:

``` r
#> # A tibble: 2 x 8
#>             timestamp    value          metric    display_name query_index interval                     host
#> *              <dttm>    <dbl>           <chr>           <chr>       <int>    <int>                    <chr>
#> 1 2015-04-29 21:50:00 98.19376 system.cpu.idle system.cpu.idle           0      600 vagrant-ubuntu-trusty-64
#> 2 2015-04-29 22:00:00 99.85857 system.cpu.idle system.cpu.idle           0      600 vagrant-ubuntu-trusty-64
#> # ... with 3 more variables: environment <chr>, role <chr>, region <chr>, expression <chr>
```

### Get Events

(This function is a but experimental.)

``` r
k9_events(start = Sys.Date() - 7, end = Sys.Date(), tags = list(role = "db"))
```
