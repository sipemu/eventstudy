# Export Event Study Results

Export event study results to CSV, Excel, or LaTeX formats.

## Usage

``` r
export_results(
  task,
  file,
  format = NULL,
  which = c("ar", "car", "aar", "model"),
  stat_name = "CSectT",
  ...
)
```

## Arguments

- task:

  A fitted EventStudyTask with statistics computed.

- file:

  Path to the output file. The file extension determines the format
  unless `format` is explicitly specified.

- format:

  Output format: "csv", "xlsx", or "latex". If NULL, inferred from the
  file extension.

- which:

  Which results to export. One or more of "ar", "car", "aar", "model".
  Defaults to all available.

- stat_name:

  Name of the multi-event test statistic to use for AAR/CAAR tables.
  Defaults to "CSectT".

- ...:

  Additional arguments passed to the format-specific writer.

## Value

The file path (invisibly).
