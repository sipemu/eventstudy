---
task_id: 260904-len
title: Multi-automaker vignette + CI bands + advisor
type: quick
version: 0.61.2
---

# Plan: Multi-automaker dieselgate dataset + CI bands + group CAAR plot + mock advisor

## Objective

Extend the advisor vignette's dieselgate walkthrough to a multi-automaker,
multi-group example (4 firms / 2 groups), add CI bands to CAR/CAAR plots,
add a multi-group CAAR comparison plot, and update the mock advisor block.
Release as 0.61.2.

## Tasks

### Task A: Extend dieselgate dataset to 4 automakers / 2 groups
- Update `data-raw/dieselgate.R` to fetch VOW.DE, PAH3.DE, BMW.DE, MBG.DE + ^GDAXI
- Build 4-row request tibble with VW Group (VOW.DE, PAH3.DE) and Other (BMW.DE, MBG.DE)
- Regenerate `data/dieselgate.rda`
- Update `R/data-dieselgate.R` roxygen doc and regenerate `man/dieselgate.Rd`

### Task B: Rewrite vignette as multi-group walkthrough
- Build 4-firm task from bundled data in `vignettes/ai-advisor.Rmd`
- Keep single-firm AR/CAR for VW (event_id 1)
- Add CAR with CI band via `plot_event_study(type="car", confidence_level=0.95)`
- Add multi-group CAAR comparison ggplot with CI ribbons
- Update mock advisor block to reflect multi-group numbers
- Update VignetteIndexEntry title to match YAML

### Task C: Release 0.61.2
- Bump DESCRIPTION Version: 0.61.1 -> 0.61.2
- Add NEWS.md entry for 0.61.2
