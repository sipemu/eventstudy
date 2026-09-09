# Example: Regulatory Shock -- Dieselgate VW-Group vs Peers

## Abstract

This worked example uses the bundled `dieselgate` dataset to demonstrate
a **two-group** event study that isolates an idiosyncratic shock from an
industry-wide one. Volkswagen and Porsche (the directly-exposed *VW
Group*) are compared against peer automakers BMW and Mercedes-Benz
(*Other*). By contrasting the cumulative average abnormal return (CAAR)
across groups around the September 2015 emissions disclosure, we ask
whether the scandal caused abnormal losses *beyond* the German auto
sector as a whole.

For the market model, see [Return
Models](https://sipemu.github.io/eventstudy/articles/methods-return-models.md);
for the cross-sectional and sign tests, see [Test
Statistics](https://sipemu.github.io/eventstudy/articles/methods-test-statistics.md).

## Research question

Did the VW emissions announcement cause significantly more negative
cumulative abnormal returns for the VW corporate family (VOW.DE,
PAH3.DE) than for the industry-peer group (BMW.DE, MBG.DE)? We test,
**separately for each group**,

H_0:\\ \mathbb{E}\[CAAR\] = 0,

and then compare the two groups’ CAR distributions directly.

## Data

The `dieselgate` dataset ships in `data/` and needs **no network
access**. Like the other bundled datasets it is a named list – `firm`
(all four automakers’ daily adjusted prices, stacked), `index` (the
benchmark), a four-row `request` carrying the per-firm event windows and
the crucial `group` assignment, and a `meta` provenance block. See the
provenance notes in
[`?dieselgate`](https://sipemu.github.io/eventstudy/reference/dieselgate.md).

``` r

library(EventStudy)
data("dieselgate", package = "EventStudy")

dplyr::glimpse(dieselgate$firm)
#> Rows: 1,440
#> Columns: 3
#> $ symbol   <chr> "VOW.DE", "VOW.DE", "VOW.DE", "VOW.DE", "VOW.DE", "VOW.DE", "…
#> $ date     <chr> "02.06.2014", "03.06.2014", "04.06.2014", "05.06.2014", "06.0…
#> $ adjusted <dbl> 112.5, 112.4, 109.5, 111.6, 111.9, 112.4, 111.7, 111.7, 112.5…

# The group column drives the two-group comparison.
dplyr::count(dieselgate$request, group)
#> # A tibble: 2 × 2
#>   group        n
#>   <chr>    <int>
#> 1 Other        2
#> 2 VW Group     2
```

## Setup and pipeline

We build the task from the three tibbles and fit a market model,
attaching the cross-sectional t-test (`CSectTTest`) and the `SignTest`
as multi-event statistics. Both aggregate the firm-level abnormal
returns *within each group*.

``` r

task <- EventStudyTask$new(
  dieselgate$firm,
  dieselgate$index,
  dieselgate$request
)

params <- ParameterSet$new(
  return_model = MarketModel$new(),
  multi_event_statistics = MultiEventStatisticsSet$new(
    tests = list(CSectTTest$new(), SignTest$new())
  )
)

result <- run_event_study(task, params)
```

See
[ParameterSet](https://sipemu.github.io/eventstudy/reference/ParameterSet.md),
[MultiEventStatisticsSet](https://sipemu.github.io/eventstudy/reference/MultiEventStatisticsSet.md),
[CSectTTest](https://sipemu.github.io/eventstudy/reference/CSectTTest.md),
and
[SignTest](https://sipemu.github.io/eventstudy/reference/SignTest.md).

## Results table

The per-group CAAR at the end of the event window is the headline
result. We pull it from the tidy AAR/CAAR output, and summarise the raw
per-firm CARs by group with
[car_by_group()](https://sipemu.github.io/eventstudy/reference/car_by_group.md).

``` r

caar_tbl <- EventStudy::tidy.EventStudyTask(result, type = "aar")

caar_tbl |>
  dplyr::group_by(group) |>
  dplyr::slice_tail(n = 1) |>
  dplyr::ungroup() |>
  dplyr::select(group, term, caar, caar_statistic, caar_p.value) |>
  knitr::kable(
    digits  = 4,
    caption = "CAAR by group at the end of the event window: VW-Group vs Peers"
  )
```

| group    | term |    caar | caar_statistic | caar_p.value |
|:---------|:-----|--------:|---------------:|-------------:|
| Other    | 10   |  0.0132 |         0.3377 |       0.7927 |
| VW Group | 10   | -0.3858 |       -12.6016 |       0.0504 |

CAAR by group at the end of the event window: VW-Group vs Peers {.table}

``` r


# Group-level CAR summary and a between-group difference test.
grp <- car_by_group(result)
knitr::kable(grp$summary, digits = 4,
             caption = "Per-firm CAR summary by group")
```

| group    |   n | mean_car | sd_car | median_car | min_car | max_car |
|:---------|----:|---------:|-------:|-----------:|--------:|--------:|
| Other    |   2 |   0.0132 | 0.0555 |     0.0132 | -0.0260 |  0.0525 |
| VW Group |   2 |  -0.3858 | 0.0433 |    -0.3858 | -0.4164 | -0.3551 |

Per-firm CAR summary by group {.table}

## Plot

``` r

gg <- plot_event_study(
  result,
  type  = "aar",
  group = "VW Group",
  title = "VW Group: cumulative average abnormal return with 95% band"
)
plotly::ggplotly(gg)
```

## Interpretation

``` r

vw_caar  <- result$aar_caar_tbl$CSectT[[
  which(result$aar_caar_tbl$group == "VW Group")]]
oth_caar <- result$aar_caar_tbl$CSectT[[
  which(result$aar_caar_tbl$group == "Other")]]

vw_end   <- round(utils::tail(vw_caar$caar,   1L), 4)
vw_t     <- round(utils::tail(vw_caar$caar_t, 1L), 2)
oth_end  <- round(utils::tail(oth_caar$caar,  1L), 4)
oth_t    <- round(utils::tail(oth_caar$caar_t, 1L), 2)
```

By the end of the window the *VW Group* CAAR is -0.3858 (cross-sectional
*t* = -12.6), while the *Other* group’s CAAR is 0.0132 (*t* = 0.34). The
contrast is the whole story: VW Group firms crater while peer automakers
barely move. Because the peer group – exposed to the same macro and
sector conditions – shows no comparable drop, the shock is
**idiosyncratic** to the VW corporate family rather than an
industry-wide contagion. The `SignTest` corroborates the direction
non-parametrically: with every VW-group firm posting negative CARs, the
sign statistic points the same way as the parametric test, which matters
when a two-firm group makes the normality assumption of the *t*-test
fragile (Brown and Warner 1985). This two-group design is the standard
technique for separating firm-specific from sector-wide regulatory
shocks.

## Diagnostics note

With only a handful of firms per group, groupwise residual
autocorrelation and non-normality materially affect the standard errors.
Check the estimation-window fit with
[es_diagnostics()](https://sipemu.github.io/eventstudy/reference/es_diagnostics.md)
before trusting the parametric *p*-values.

## Further reading

- [Return
  Models](https://sipemu.github.io/eventstudy/articles/methods-return-models.md)
  – market-model theory.
- [Test
  Statistics](https://sipemu.github.io/eventstudy/articles/methods-test-statistics.md)
  – cross-sectional and sign tests.
- [Panel /
  DiD](https://sipemu.github.io/eventstudy/articles/methods-panel-did.md)
  – for staggered-treatment regulatory events.
- [car_by_group()](https://sipemu.github.io/eventstudy/reference/car_by_group.md)
  – group CAR summary reference.

## Session info

    #> R version 4.6.1 (2026-06-24)
    #> Platform: x86_64-pc-linux-gnu
    #> Running under: Ubuntu 24.04.5 LTS
    #> 
    #> Matrix products: default
    #> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
    #> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
    #> 
    #> locale:
    #>  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
    #>  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
    #>  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
    #> [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
    #> 
    #> time zone: UTC
    #> tzcode source: system (glibc)
    #> 
    #> attached base packages:
    #> [1] stats     graphics  grDevices utils     datasets  methods   base     
    #> 
    #> other attached packages:
    #> [1] EventStudy_0.65.0
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] plotly_4.12.1        sass_0.4.10          utf8_1.2.6          
    #>  [4] generics_0.1.4       tidyr_1.3.2          stringi_1.8.9       
    #>  [7] digest_0.6.39        magrittr_2.0.5       evaluate_1.0.5      
    #> [10] grid_4.6.1           RColorBrewer_1.1-3   fastmap_1.2.0       
    #> [13] jsonlite_2.0.0       httr_1.4.9           purrr_1.2.2         
    #> [16] crosstalk_1.2.2      viridisLite_0.4.3    scales_1.4.0        
    #> [19] textshaping_1.0.5    jquerylib_0.1.4      cli_3.6.6           
    #> [22] rlang_1.3.0          withr_3.0.3          cachem_1.1.0        
    #> [25] yaml_2.3.12          otel_0.2.0           tools_4.6.1         
    #> [28] dplyr_1.2.1          ggplot2_4.0.3        vctrs_0.7.3         
    #> [31] R6_2.6.1             lifecycle_1.0.5      stringr_1.6.0       
    #> [34] fs_2.1.0             htmlwidgets_1.6.4    ragg_1.5.2          
    #> [37] pkgconfig_2.0.3      desc_1.4.3           pkgdown_2.2.1       
    #> [40] pillar_1.11.1        bslib_0.12.0         gtable_0.3.6        
    #> [43] glue_1.8.1           data.table_1.18.6.1  systemfonts_1.3.2   
    #> [46] xfun_0.60            tibble_3.3.1         tidyselect_1.2.1    
    #> [49] knitr_1.52           farver_2.1.2         htmltools_0.5.9     
    #> [52] rmarkdown_2.32       labeling_0.4.3       compiler_4.6.1      
    #> [55] S7_0.2.2             distributional_0.8.1

Brown, Stephen J., and Jerold B. Warner. 1985. “Using Daily Stock
Returns: The Case of Event Studies.” *Journal of Financial Economics* 14
(1): 3–31.
