# Advisor Pro — Future Retrieval-Grounded Paid Tier

**Advisor Pro** is a planned premium add-on for the EventStudy package
that will provide retrieval-augmented, evidence-grounded AI advice
backed by a curated academic knowledge base. It is not yet available;
this help topic documents the waitlist mechanism.

## What Advisor Pro will include

- Retrieval-grounded recommendations sourced from peer-reviewed
  methodology (MacKinlay 1997, Fama 1991, Boehmer et al. 1991, and more)

- Automatic context assembly from
  [`es_diagnostics`](https://sipemu.github.io/eventstudy/reference/es_diagnostics.md)
  output

- Structured `Advice` objects with traceable evidence chains

- Priority access to new KB rules and model integrations

## Current offline tier

The current package already ships an offline advice layer based on a
deterministic knowledge base:

- [`recommend_stat()`](https://sipemu.github.io/eventstudy/reference/recommend_stat.md)
  — KB-based test-statistic recommendation

- [`flag_robustness()`](https://sipemu.github.io/eventstudy/reference/flag_robustness.md)
  — KB-based robustness flags

- [`es_advise()`](https://sipemu.github.io/eventstudy/reference/es_advise.md)
  — LLM-backed advice (requires provider)

## Waitlist

To join the Advisor Pro waitlist, visit:
<https://github.com/sipemu/eventstudy#advisor-pro-waitlist>

To enable an optional footer reminder after printing advice objects:

    options(eventstudy.advisor_pro_footer = TRUE)

The footer is silent by default; enabling it appends a static URL — no
network connection is made.
