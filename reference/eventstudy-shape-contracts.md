# Return-Shape Contracts for EventStudy Pipeline Results

The shape-contract system locks the column names and types of the
EventStudy pipeline's result tibbles against accidental structural
drift. It is designed for development and CI use only.

**Default:** OFF. When the option is unset (the default) the entire
shape-contract path is a strict no-op — every existing caller and the
full test suite are completely unaffected.

**Opt-in:** `options(EventStudy.shape_contracts = TRUE)`

**Philosophy (mirrors the degenerate-input contract):** On a column-name
or type mismatch the contract emits exactly *one*
[`warning()`](https://rdrr.io/r/base/warning.html), naming the context
and the specific drift, and then returns invisibly. It **never** calls
[`stop()`](https://rdrr.io/r/base/stop.html). The correctly-shaped
`is_fitted = FALSE` degenerate output (same column names,
`NA`-propagated values) is treated as a *valid* shape and produces no
warning.

**Coverage:**

- Single-event statistics tibbles: `ART` (ARTTest) and `CART`
  (CARTTest), including their `is_fitted = FALSE` degenerate variants.

- Multi-event AAR/CAAR statistics tibble: `CSectT` (CSectTTest) and all
  other multi-event result tibbles, including their `is_fitted = FALSE`
  degenerate variants.

**Configuration:**

- Via package option: `options(EventStudy.shape_contracts = TRUE)`

- Default is `FALSE` (off).

## Details

Return-Shape Contracts for EventStudy Pipeline Results

## See also

[`degenerate-input-contract`](https://sipemu.github.io/eventstudy/reference/degenerate-input-contract.md),
[`ParameterSet`](https://sipemu.github.io/eventstudy/reference/ParameterSet.md)
