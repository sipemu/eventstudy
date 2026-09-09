# Example: Earnings Surprise Analysis (AAPL, MSFT, GOOGL)

## Abstract

This worked example takes you from the bundled `earnings_surprises`
dataset – three US mega-caps (AAPL, MSFT, GOOGL) around their Q1-2023
analyst-beat earnings dates – through a complete market-model event
study: data loading, pipeline execution, cross-sectional and
standardized-residual test statistics (Patell Z and BMP), and a rendered
cumulative-abnormal-return (CAR) plot with written interpretation.

For the theory behind the market model, see [Return
Models](https://sipemu.github.io/eventstudy/articles/methods-return-models.md);
for the Patell Z and BMP tests, see [Test
Statistics](https://sipemu.github.io/eventstudy/articles/methods-test-statistics.md).

## Research question

Do AAPL, MSFT, and GOOGL earn significantly positive abnormal returns
around their quarterly earnings *beats*? Formally, we test the null

H_0:\\ \mathbb{E}\[AAR_t\] = 0 \quad \text{for all } t \text{ in the
event window},

against the alternative that the cumulative average abnormal return
(CAAR) differs from zero.

## Data

The `earnings_surprises` dataset ships with the package (in `data/`) and
requires **no network access**. It is a named list of three tibbles –
`firm` (stacked daily adjusted prices for the three firms), `index` (the
S&P 500 benchmark), and `request` (the event specification: one row per
firm with event date and estimation/event windows) – plus a `meta`
provenance block.

[`library`](https://rdrr.io/r/base/library.html)`(`[`EventStudy`](https://github.com/sipemu/eventstudy)`)`` `[`data`](https://rdrr.io/r/utils/data.html)`(``"earnings_surprises"``, package ``=`` ``"EventStudy"``)`` `` ``# The dataset is a list of tibbles; inspect the pieces we consume.`` ``dplyr``::`[`glimpse`](https://pillar.r-lib.org/reference/glimpse.html)`(``earnings_surprises``$``firm``)`` ``#> Rows: 813`` ``#> Columns: 3`` ``#> $ symbol ``<chr>`` "AAPL"``, ``"AAPL"``, ``"AAPL"``, ``"AAPL"``, ``"AAPL"``, ``"AAPL"``, ``"AAPL"``, ``"AAPL…`` ``#> $ date ``<chr>`` "01.06.2022"``, ``"02.06.2022"``, ``"03.06.2022"``, ``"06.06.2022"``, ``"07.0…`` ``#> $ adjusted ``<dbl>`` 145.7``, ``148.1``, ``142.4``, ``143.1``, ``145.7``, ``144.9``, ``139.7``, ``134.3``, ``129.2…`` ``earnings_surprises``$``request``[``, `[`c`](https://rdrr.io/r/base/c.html)`(`` `` ``"event_id"``, ``"firm_symbol"``, ``"index_symbol"``, ``"event_date"``,`` `` ``"group"``, ``"event_window_start"``, ``"event_window_end"``,`` `` ``"estimation_window_length"`` ``)``]`` ``#> ``# A tibble: 3 × 8`` ``#> event_id firm_symbol index_symbol event_date group event_window_start`` ``#> ``<int>`` ``<chr>`` ``<chr>`` ``<chr>`` ``<chr>`` ``<int>`` ``#> ``1`` 1 AAPL ^GSPC 04.05.2023 Earnings Beat -``5`` ``#> ``2`` 2 MSFT ^GSPC 25.04.2023 Earnings Beat -``5`` ``#> ``3`` 3 GOOGL ^GSPC 25.04.2023 Earnings Beat -``5`` ``#> ``# ℹ 2 more variables: event_window_end <int>, estimation_window_length <int>`

All three events belong to a single `"Earnings Beat"` group, which lets
the multi-event statistics aggregate them into one AAR/CAAR series.

## Setup and pipeline

`EventStudyTask$new()` takes the three tibbles positionally – firm
prices, reference (index) prices, and the request table. We fit a
`MarketModel` and attach the multi-event statistics we care about
(`CSectTTest`, `PatellZTest`, `BMPTest`) via a
`MultiEventStatisticsSet`.

`task`` ``<-`` `[`EventStudyTask`](https://sipemu.github.io/eventstudy/reference/EventStudyTask.md)`$``new``(`` `` ``earnings_surprises``$``firm``, ``# firm daily adjusted prices`` `` ``earnings_surprises``$``index``, ``# benchmark index prices`` `` ``earnings_surprises``$``request`` ``# event / window specification`` ``)`` `` ``params`` ``<-`` `[`ParameterSet`](https://sipemu.github.io/eventstudy/reference/ParameterSet.md)`$``new``(`` `` return_model ``=`` `[`MarketModel`](https://sipemu.github.io/eventstudy/reference/MarketModel.md)`$``new``(``)``,`` `` multi_event_statistics ``=`` `[`MultiEventStatisticsSet`](https://sipemu.github.io/eventstudy/reference/MultiEventStatisticsSet.md)`$``new``(`` `` tests ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`[`CSectTTest`](https://sipemu.github.io/eventstudy/reference/CSectTTest.md)`$``new``(``)``, `[`PatellZTest`](https://sipemu.github.io/eventstudy/reference/PatellZTest.md)`$``new``(``)``, `[`BMPTest`](https://sipemu.github.io/eventstudy/reference/BMPTest.md)`$``new``(``)``)`` `` ``)`` ``)`` `` ``result`` ``<-`` `[`run_event_study`](https://sipemu.github.io/eventstudy/reference/run_event_study.md)`(``task``, ``params``)`

The pipeline builds the task, fits the market model on each firm’s
estimation window, computes abnormal returns over the event window, and
evaluates the statistics. See
[run_event_study()](https://sipemu.github.io/eventstudy/reference/run_event_study.md),
[EventStudyTask](https://sipemu.github.io/eventstudy/reference/EventStudyTask.md),
[ParameterSet](https://sipemu.github.io/eventstudy/reference/ParameterSet.md),
[MarketModel](https://sipemu.github.io/eventstudy/reference/MarketModel.md),
[PatellZTest](https://sipemu.github.io/eventstudy/reference/PatellZTest.md),
and [BMPTest](https://sipemu.github.io/eventstudy/reference/BMPTest.md).

## Results table

The `tidy()` method returns per-firm cumulative abnormal returns as a
tidy tibble. We call the S3 method explicitly
([`tidy.EventStudyTask()`](https://sipemu.github.io/eventstudy/reference/tidy.EventStudyTask.md)),
since the `broom` generic is not re-exported by the package.

`car_tbl`` ``<-`` ``EventStudy``::`[`tidy.EventStudyTask`](https://sipemu.github.io/eventstudy/reference/tidy.EventStudyTask.md)`(``result``, type ``=`` ``"car"``)`` `` ``# Show the last (widest) CAR window per firm.`` ``car_tbl`` ``|>`` `` ``dplyr``::`[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``firm_symbol``)`` ``|>`` `` ``dplyr``::`[`slice_tail`](https://dplyr.tidyverse.org/reference/slice.html)`(``n ``=`` ``1``)`` ``|>`` `` ``dplyr``::`[`ungroup`](https://dplyr.tidyverse.org/reference/group_by.html)`(``)`` ``|>`` `` ``dplyr``::`[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``firm_symbol``, ``term``, ``estimate``, ``std.error``, ``statistic``, ``p.value``)`` ``|>`` `` ``es_tt``(`` `` digits ``=`` ``4``,`` `` caption ``=`` ``"Cumulative Abnormal Returns by firm (widest event window)"`` `` ``)`

| firm_symbol | term     | estimate | std.error | statistic | p.value |
|-------------|----------|----------|-----------|-----------|---------|
| AAPL        | \[-5,5\] | 0.03232  | 0.03533   | 0.915     | 0.36133 |
| GOOGL       | \[-5,5\] | 0.01302  | 0.05171   | 0.2518    | 0.80143 |
| MSFT        | \[-5,5\] | 0.06781  | 0.03889   | 1.7436    | 0.08278 |

Cumulative Abnormal Returns by firm (widest event window)
{#tinytable_11fabb67fc3odzvtbqza .table .tinytable
style="width: auto; margin-left: auto; margin-right: auto;"
quarto-disable-processing="true"}

## Plot

[`plot_event_study()`](https://sipemu.github.io/eventstudy/reference/plot_event_study.md)
returns a `ggplot`; wrapping it with
[`plotly::ggplotly()`](https://rdrr.io/pkg/plotly/man/ggplotly.html)
produces an interactive plot for the article.

`gg`` ``<-`` `[`plot_event_study`](https://sipemu.github.io/eventstudy/reference/plot_event_study.md)`(`` `` ``result``,`` `` type ``=`` ``"car"``,`` `` event_id ``=`` ``1L``,`` `` title ``=`` ``"AAPL: cumulative abnormal return with 95% confidence band"`` ``)`` ``plotly``::`[`ggplotly`](https://rdrr.io/pkg/plotly/man/ggplotly.html)`(``gg``)`

## Statistical interpretation

The Patell Z and BMP statistics both aggregate the three firms’
standardized abnormal returns, but BMP additionally corrects for
event-induced cross-sectional variance.

`pz`` ``<-`` ``result``$``aar_caar_tbl``$``PatellZ``[[``1``]``]`` ``bmp`` ``<-`` ``result``$``aar_caar_tbl``$``BMP``[[``1``]``]`` `` ``caar_patell`` ``<-`` `[`round`](https://rdrr.io/r/base/Round.html)`(``utils``::`[`tail`](https://rdrr.io/r/utils/head.html)`(``pz``$``caar``, ``1L``)``, ``4``)`` ``z_patell`` ``<-`` `[`round`](https://rdrr.io/r/base/Round.html)`(``utils``::`[`tail`](https://rdrr.io/r/utils/head.html)`(``pz``$``caar_z``, ``1L``)``, ``3``)`` ``t_bmp`` ``<-`` `[`round`](https://rdrr.io/r/base/Round.html)`(``utils``::`[`tail`](https://rdrr.io/r/utils/head.html)`(``bmp``$``cbmp_t``, ``1L``)``, ``3``)`` ``df_bmp`` ``<-`` ``utils``::`[`tail`](https://rdrr.io/r/utils/head.html)`(``bmp``$``n_valid_events``, ``1L``)`` ``-`` ``1L`` `` ``# Two-sided p-values: Patell Z ~ N(0,1); BMP t ~ t(df_bmp)`` ``p_patell`` ``<-`` `[`round`](https://rdrr.io/r/base/Round.html)`(``2`` ``*`` `[`pnorm`](https://rdrr.io/r/stats/Normal.html)`(`[`abs`](https://rdrr.io/r/base/MathFun.html)`(``z_patell``)``, lower.tail ``=`` ``FALSE``)``, ``4``)`` ``p_bmp`` ``<-`` `[`round`](https://rdrr.io/r/base/Round.html)`(``2`` ``*`` `[`pt`](https://rdrr.io/r/stats/TDist.html)`(`[`abs`](https://rdrr.io/r/base/MathFun.html)`(``t_bmp``)``, df ``=`` ``df_bmp``, lower.tail ``=`` ``FALSE``)``, ``4``)`

At the end of the event window the CAAR across the three firms is
0.0377. The Patell Z on the cumulative series is 1.664 (two-sided p =
0.0961) and the BMP statistic is 2.248 (two-sided p = 0.1536). Where the
two disagree in magnitude, the BMP figure is the more conservative – it
inflates the standard error when abnormal returns are correlated across
firms on the event day, which is exactly the situation a common
earnings-season calendar can create (Boehmer et al. 1991). Read
together, they tell you whether an apparently significant AAR survives a
cross-sectional correlation correction.

## Diagnostics note

The market-model OLS residuals underpinning these statistics can be
tested for autocorrelation and non-normality with
[es_diagnostics()](https://sipemu.github.io/eventstudy/reference/es_diagnostics.md);
a poor estimation-window fit weakens every downstream statistic. See
[Diagnostics &
Robustness](https://sipemu.github.io/eventstudy/articles/methods-diagnostics.md)
for the full battery.

## Further reading

- [Return
  Models](https://sipemu.github.io/eventstudy/articles/methods-return-models.md)
  – market-model theory and alternatives.
- [Test
  Statistics](https://sipemu.github.io/eventstudy/articles/methods-test-statistics.md)
  – Patell Z and BMP derivations.
- [run_event_study()](https://sipemu.github.io/eventstudy/reference/run_event_study.md)
  – pipeline reference page.

## Session info

    #> R version 4.6.1 (2026-06-24)
    #> Platform: x86_64-pc-linux-gnu
    #> Running under: Manjaro Linux
    #> 
    #> Matrix products: default
    #> BLAS:   /usr/lib/libblas.so.3.12.0 
    #> LAPACK: /usr/lib/liblapack.so.3.12.0  LAPACK version 3.12.0
    #> 
    #> locale:
    #>  [1] LC_CTYPE=de_DE.UTF-8       LC_NUMERIC=C              
    #>  [3] LC_TIME=de_DE.UTF-8        LC_COLLATE=de_DE.UTF-8    
    #>  [5] LC_MONETARY=de_DE.UTF-8    LC_MESSAGES=de_DE.UTF-8   
    #>  [7] LC_PAPER=de_DE.UTF-8       LC_NAME=C                 
    #>  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
    #> [11] LC_MEASUREMENT=de_DE.UTF-8 LC_IDENTIFICATION=C       
    #> 
    #> time zone: Europe/Amsterdam
    #> tzcode source: system (glibc)
    #> 
    #> attached base packages:
    #> [1] stats     graphics  grDevices utils     datasets  methods   base     
    #> 
    #> other attached packages:
    #> [1] EventStudy_0.65.0
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] plotly_4.12.0        sass_0.4.10          utf8_1.2.6          
    #>  [4] generics_0.1.4       tidyr_1.3.2          stringi_1.8.7       
    #>  [7] digest_0.6.39        magrittr_2.0.5       evaluate_1.0.5      
    #> [10] grid_4.6.1           RColorBrewer_1.1-3   fastmap_1.2.0       
    #> [13] jsonlite_2.0.0       httr_1.4.8           purrr_1.2.2         
    #> [16] crosstalk_1.2.2      viridisLite_0.4.3    scales_1.4.0        
    #> [19] lazyeval_0.2.3       textshaping_1.0.5    jquerylib_0.1.4     
    #> [22] cli_3.6.6            rlang_1.2.0          withr_3.0.3         
    #> [25] cachem_1.1.0         yaml_2.3.12          tinytable_0.18.0    
    #> [28] otel_0.2.0           tools_4.6.1          dplyr_1.2.1         
    #> [31] ggplot2_4.0.3        vctrs_0.7.3          R6_2.6.1            
    #> [34] lifecycle_1.0.5      stringr_1.6.0        fs_2.1.0            
    #> [37] htmlwidgets_1.6.4    ragg_1.5.2           pkgconfig_2.0.3     
    #> [40] desc_1.4.3           pkgdown_2.2.0        pillar_1.11.1       
    #> [43] bslib_0.11.0         gtable_0.3.6         glue_1.8.1          
    #> [46] data.table_1.18.4    systemfonts_1.3.2    xfun_0.59           
    #> [49] tibble_3.3.1         tidyselect_1.2.1     knitr_1.51          
    #> [52] farver_2.1.2         htmltools_0.5.9      labeling_0.4.3      
    #> [55] rmarkdown_2.31       compiler_4.6.1       S7_0.2.2            
    #> [58] distributional_0.8.0

Boehmer, Ekkehart, Jim Musumeci, and Annette B. Poulsen. 1991.
“Event-Study Methodology Under Conditions of Event-Induced Variance.”
*Journal of Financial Economics* 30 (2): 253–72.
