# Wild Bootstrap Inference for Event Studies

Computes bootstrap p-values for AAR and CAAR test statistics using the
wild bootstrap approach. Per-firm random weights preserve the
cross-sectional dependence structure while randomizing the sign of
abnormal returns under the null hypothesis.

## Usage

``` r
bootstrap_test(
  task,
  n_boot = 999L,
  weight_type = "rademacher",
  statistic = "both",
  group = NULL,
  seed = NULL
)
```

## Arguments

- task:

  A fitted EventStudyTask with abnormal returns computed.

- n_boot:

  Number of bootstrap replications. Default 999.

- weight_type:

  Type of bootstrap weights: `"rademacher"` (default, +1/-1 with equal
  probability) or `"mammen"` (Mammen two-point distribution).

- statistic:

  Which statistic to bootstrap: `"aar"`, `"caar"`, or `"both"`
  (default).

- group:

  Optional group name to filter.

- seed:

  Optional seed for reproducibility.

## Value

A tibble with columns: `relative_index`, `observed_aar`,
`observed_caar`, `boot_p_aar`, `boot_p_caar`.
