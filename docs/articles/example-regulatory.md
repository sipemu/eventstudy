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

[`library`](https://rdrr.io/r/base/library.html)`(`[`EventStudy`](https://github.com/sipemu/eventstudy)`)`` `[`data`](https://rdrr.io/r/utils/data.html)`(``"dieselgate"``, package ``=`` ``"EventStudy"``)`` `` ``dplyr``::`[`glimpse`](https://pillar.r-lib.org/reference/glimpse.html)`(``dieselgate``$``firm``)`` ``#> Rows: 1,440`` ``#> Columns: 3`` ``#> $ symbol ``<chr>`` "VOW.DE"``, ``"VOW.DE"``, ``"VOW.DE"``, ``"VOW.DE"``, ``"VOW.DE"``, ``"VOW.DE"``, ``"…`` ``#> $ date ``<chr>`` "02.06.2014"``, ``"03.06.2014"``, ``"04.06.2014"``, ``"05.06.2014"``, ``"06.0…`` ``#> $ adjusted ``<dbl>`` 112.5``, ``112.4``, ``109.5``, ``111.6``, ``111.9``, ``112.4``, ``111.7``, ``111.7``, ``112.5…`` `` ``# The group column drives the two-group comparison.`` ``dplyr``::`[`count`](https://dplyr.tidyverse.org/reference/count.html)`(``dieselgate``$``request``, ``group``)`` ``#> ``# A tibble: 2 × 2`` ``#> group n`` ``#> ``<chr>`` ``<int>`` ``#> ``1`` Other 2`` ``#> ``2`` VW Group 2`

## Setup and pipeline

We build the task from the three tibbles and fit a market model,
attaching the cross-sectional t-test (`CSectTTest`) and the `SignTest`
as multi-event statistics. Both aggregate the firm-level abnormal
returns *within each group*.

`task`` ``<-`` `[`EventStudyTask`](https://sipemu.github.io/eventstudy/reference/EventStudyTask.md)`$``new``(`` `` ``dieselgate``$``firm``,`` `` ``dieselgate``$``index``,`` `` ``dieselgate``$``request`` ``)`` `` ``params`` ``<-`` `[`ParameterSet`](https://sipemu.github.io/eventstudy/reference/ParameterSet.md)`$``new``(`` `` return_model ``=`` `[`MarketModel`](https://sipemu.github.io/eventstudy/reference/MarketModel.md)`$``new``(``)``,`` `` multi_event_statistics ``=`` `[`MultiEventStatisticsSet`](https://sipemu.github.io/eventstudy/reference/MultiEventStatisticsSet.md)`$``new``(`` `` tests ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`[`CSectTTest`](https://sipemu.github.io/eventstudy/reference/CSectTTest.md)`$``new``(``)``, `[`SignTest`](https://sipemu.github.io/eventstudy/reference/SignTest.md)`$``new``(``)``)`` `` ``)`` ``)`` `` ``result`` ``<-`` `[`run_event_study`](https://sipemu.github.io/eventstudy/reference/run_event_study.md)`(``task``, ``params``)`

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

`caar_tbl`` ``<-`` ``EventStudy``::`[`tidy.EventStudyTask`](https://sipemu.github.io/eventstudy/reference/tidy.EventStudyTask.md)`(``result``, type ``=`` ``"aar"``)`` `` ``caar_tbl`` ``|>`` `` ``dplyr``::`[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``group``)`` ``|>`` `` ``dplyr``::`[`slice_tail`](https://dplyr.tidyverse.org/reference/slice.html)`(``n ``=`` ``1``)`` ``|>`` `` ``dplyr``::`[`ungroup`](https://dplyr.tidyverse.org/reference/group_by.html)`(``)`` ``|>`` `` ``dplyr``::`[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``group``, ``term``, ``caar``, ``caar_statistic``, ``caar_p.value``)`` ``|>`` `` ``es_tt``(`` `` digits ``=`` ``4``,`` `` caption ``=`` ``"CAAR by group at the end of the event window: VW-Group vs Peers"`` `` ``)`

[TABLE]

CAAR by group at the end of the event window: VW-Group vs Peers
{#tinytable_ogc59huwyk8pey51nq6b .table .tinytable
style="width: auto; margin-left: auto; margin-right: auto;"
quarto-disable-processing="true"}

` ``# Group-level CAR summary and a between-group difference test.`` ``grp`` ``<-`` `[`car_by_group`](https://sipemu.github.io/eventstudy/reference/car_by_group.md)`(``result``)`` ``es_tt``(``grp``$``summary``, digits ``=`` ``4``,`` `` caption ``=`` ``"Per-firm CAR summary by group"``)`

| group    | n   | mean_car | sd_car  | median_car | min_car  | max_car  |
|----------|-----|----------|---------|------------|----------|----------|
| Other    | 2   | 0.01324  | 0.05546 | 0.01324    | -0.02597 | 0.05246  |
| VW Group | 2   | -0.38576 | 0.04329 | -0.38576   | -0.41637 | -0.35515 |

Per-firm CAR summary by group {#tinytable_7li7krvamoc074i3fdol .table
.tinytable style="width: auto; margin-left: auto; margin-right: auto;"
quarto-disable-processing="true"}

## Plot

`gg`` ``<-`` `[`plot_event_study`](https://sipemu.github.io/eventstudy/reference/plot_event_study.md)`(`` `` ``result``,`` `` type ``=`` ``"aar"``,`` `` group ``=`` ``"VW Group"``,`` `` title ``=`` ``"VW Group: cumulative average abnormal return with 95% band"`` ``)`` ``plotly``::`[`ggplotly`](https://rdrr.io/pkg/plotly/man/ggplotly.html)`(``gg``)`

## Interpretation

`vw_caar`` ``<-`` ``result``$``aar_caar_tbl``$``CSectT``[[`` `` `[`which`](https://rdrr.io/r/base/which.html)`(``result``$``aar_caar_tbl``$``group`` ``==`` ``"VW Group"``)``]``]`` ``oth_caar`` ``<-`` ``result``$``aar_caar_tbl``$``CSectT``[[`` `` `[`which`](https://rdrr.io/r/base/which.html)`(``result``$``aar_caar_tbl``$``group`` ``==`` ``"Other"``)``]``]`` `` ``vw_end`` ``<-`` `[`round`](https://rdrr.io/r/base/Round.html)`(``utils``::`[`tail`](https://rdrr.io/r/utils/head.html)`(``vw_caar``$``caar``, ``1L``)``, ``4``)`` ``vw_t`` ``<-`` `[`round`](https://rdrr.io/r/base/Round.html)`(``utils``::`[`tail`](https://rdrr.io/r/utils/head.html)`(``vw_caar``$``caar_t``, ``1L``)``, ``2``)`` ``oth_end`` ``<-`` `[`round`](https://rdrr.io/r/base/Round.html)`(``utils``::`[`tail`](https://rdrr.io/r/utils/head.html)`(``oth_caar``$``caar``, ``1L``)``, ``4``)`` ``oth_t`` ``<-`` `[`round`](https://rdrr.io/r/base/Round.html)`(``utils``::`[`tail`](https://rdrr.io/r/utils/head.html)`(``oth_caar``$``caar_t``, ``1L``)``, ``2``)`

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

Brown, Stephen J., and Jerold B. Warner. 1985. “Using Daily Stock
Returns: The Case of Event Studies.” *Journal of Financial Economics* 14
(1): 3–31.
