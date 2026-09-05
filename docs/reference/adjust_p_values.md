# Adjust P-Values for Multiple Testing

Computes adjusted p-values for AAR and CAAR test statistics across the
event window, correcting for the multiple comparisons problem. Supports
all methods available in
[`p.adjust`](https://rdrr.io/r/stats/p.adjust.html).

## Usage

``` r
adjust_p_values(task, method = "BH", stat_name = "CSectT", group = NULL)
```

## Arguments

- task:

  A fitted EventStudyTask with `aar_caar_tbl` populated.

- method:

  Adjustment method passed to
  [`p.adjust`](https://rdrr.io/r/stats/p.adjust.html). Common choices:
  `"BH"` (Benjamini-Hochberg, default), `"bonferroni"`, `"holm"`,
  `"hochberg"`, `"BY"`, `"none"`.

- stat_name:

  Name of the multi-event test statistic to adjust. Must match a column
  name in `task$aar_caar_tbl`. Default `"CSectT"`.

- group:

  Optional group name to filter. If NULL, adjusts all groups.

## Value

A tibble with columns from the original test statistic result plus
`p_raw_aar`, `p_adj_aar`, `p_raw_caar`, `p_adj_caar`.
