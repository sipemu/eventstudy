# APIS-03: structural API surface snapshot (exports + formals + S3 methods)

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["exports", "functions", "r6_classes", "other_exports", "s3_methods"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["ARTTest", "AnthropicProvider", "BHARModel", "BHARTTest", "BMPTest", "CARTTest", "CSectTTest", "CalendarTimePortfolioTest", "Carhart4FactorModel", "ComparisonPeriodMeanAdjustedModel", "CustomProvider", "DCCGARCHModel", "EventStudyTask", "FamaFrench3FactorModel", "FamaFrench5FactorModel", "GARCHModel", "GeneralizedSignTest", "IntradayEventStudyTask", "KolariPynnonenTest", "LinearFactorModel", "LogReturn", "MarketAdjustedModel", "MarketModel", "ModelBase", "MultiEventStatisticsSet", "OpenAICompatProvider", "PanelEventStudyTask", "ParameterSet", "PatellZTest", "ProviderBase", "RankTest", "ReturnCalculation", "RollingWindowModel", "SignTest", "SimpleReturn", "SingleEventStatisticsSet", "StatisticsSetBase", "SyntheticControlTask", "TestStatisticBase", "VolatilityModel", "VolumeModel", "adjust_p_values", "bootstrap_test", "calculate_statistics", "car_by_group", "car_quantiles", "cross_sectional_regression", "download_factor_data", "download_risk_free_rate", "download_stock_data", "es_advise", "es_colours", "es_diagnostics", "es_kb", "es_report", "estimate_panel_event_study", "estimate_synthetic_control", "export_results", "fit_model", "flag_robustness", "generate_report", "model_diagnostics", "nonparametric_intraday_test", "plot_car_distribution", "plot_diagnostics", "plot_event_study", "plot_panel_event_study", "plot_stocks", "plot_synthetic_control", "prepare_event_study", "prepare_intraday_event_study", "pretrend_test", "provider", "recommend_stat", "report_table", "run_event_study", "sc_placebo_test", "simulate_event_study", "theme_eventstudy", "tidy.EventStudyTask", "validate_task"]
        },
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["adjust_p_values", "bootstrap_test", "calculate_statistics", "car_by_group", "car_quantiles", "cross_sectional_regression", "download_factor_data", "download_risk_free_rate", "download_stock_data", "es_advise", "es_diagnostics", "es_kb", "es_report", "estimate_panel_event_study", "estimate_synthetic_control", "export_results", "fit_model", "flag_robustness", "generate_report", "model_diagnostics", "nonparametric_intraday_test", "plot_car_distribution", "plot_diagnostics", "plot_event_study", "plot_panel_event_study", "plot_stocks", "plot_synthetic_control", "prepare_event_study", "prepare_intraday_event_study", "pretrend_test", "provider", "recommend_stat", "report_table", "run_event_study", "sc_placebo_test", "simulate_event_study", "theme_eventstudy", "tidy.EventStudyTask", "validate_task"]
            }
          },
          "value": [
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["group", "method", "stat_name", "task"]
                }
              },
              "value": [
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["NULL"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["\"BH\""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["\"CSectT\""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["group", "n_boot", "seed", "statistic", "task", "weight_type"]
                }
              },
              "value": [
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["NULL"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["999L"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["NULL"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["\"both\""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["\"rademacher\""]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["parameter_set", "task"]
                }
              },
              "value": [
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["car_window", "group_var", "task"]
                }
              },
              "value": [
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["NULL"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["\"group\""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["car_window", "probs", "task"]
                }
              },
              "value": [
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["NULL"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["c(0.05, 0.25, 0.5, 0.75, 0.95)"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["car_window", "data", "formula", "robust", "task"]
                }
              },
              "value": [
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["NULL"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["TRUE"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["format_for_task", "frequency", "model"]
                }
              },
              "value": [
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["TRUE"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["c(\"daily\", \"monthly\")"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["c(\"ff3\", \"ff5\", \"mom\")"]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["format_for_task", "frequency", "source"]
                }
              },
              "value": [
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["TRUE"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["c(\"daily\", \"monthly\")"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["\"french\""]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["format_for_task", "from", "source", "symbols", "to"]
                }
              },
              "value": [
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["TRUE"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["\"yahoo\""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["Sys.Date()"]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["...", "diagnostics", "model", "provider", "section_hint", "task_type"]
                }
              },
              "value": [
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["NULL"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["NULL"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["NULL"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["max_events", "task"]
                }
              },
              "value": [
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["20L"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {},
              "value": []
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["...", "author", "confidence_level", "format", "interactive", "output_file", "provider", "sections", "task", "title", "verbose"]
                }
              },
              "value": [
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["NULL"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["0.95"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["\"html\""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["TRUE"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["\"event_study_report.html\""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["NULL"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["c(\"exec_summary\", \"data_methods\", \"results\", \"diagnostics\", \"robustness\", ", "    \"references\")"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["\"Event Study Report\""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["getOption(\"eventstudy.verbose\", TRUE)"]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["...", "base_period", "cluster", "lags", "leads", "method", "task", "verbose"]
                }
              },
              "value": [
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["-1"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["NULL"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["5"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["5"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["c(\"static_twfe\", \"dynamic_twfe\", \"sun_abraham\", \"callaway_santanna\", ", "    \"dechaisemartin_dhaultfoeuille\", \"borusyak_jaravel_spiess\")"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["getOption(\"eventstudy.verbose\", TRUE)"]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["covariates", "method", "task", "verbose"]
                }
              },
              "value": [
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["NULL"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["c(\"quadprog\", \"optim\")"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["getOption(\"eventstudy.verbose\", TRUE)"]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["...", "file", "format", "stat_name", "task", "which"]
                }
              },
              "value": [
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["NULL"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["\"CSectT\""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["c(\"ar\", \"car\", \"aar\", \"model\")"]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["parameter_set", "task"]
                }
              },
              "value": [
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["...", "provider", "x"]
                }
              },
              "value": [
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["NULL"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["...", "advice", "author", "confidence_level", "cross_sectional", "format", "interactive", "narrative", "output_file", "provider", "sections", "task", "title", "verbose"]
                }
              },
              "value": [
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["NULL"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["NULL"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["0.95"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["NULL"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["\"html\""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["TRUE"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["NULL"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["\"event_study_report.html\""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["NULL"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["c(\"exec_summary\", \"data_methods\", \"results\", \"diagnostics\", \"robustness\", ", "    \"references\")"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["\"Event Study Report\""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["getOption(\"eventstudy.verbose\", TRUE)"]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["event_id", "task"]
                }
              },
              "value": [
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["NULL"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["estimation_window", "event_times", "event_window", "init_window", "p", "upper"]
                }
              },
              "value": [
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["5L"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["0.05"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["FALSE"]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["bins", "by_group", "car_window", "task", "title"]
                }
              },
              "value": [
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["30"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["FALSE"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["NULL"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["NULL"]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["event_id", "task"]
                }
              },
              "value": [
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["NULL"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["confidence_level", "event_id", "group", "stat_name", "task", "title", "type"]
                }
              },
              "value": [
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["0.95"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["NULL"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["NULL"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["\"CSectT\""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["NULL"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["\"car\""]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["confidence_level", "task", "title"]
                }
              },
              "value": [
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["0.95"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["NULL"]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["add_event_date", "do_sample", "max_symbols", "sample_symbols", "target_variable", "task"]
                }
              },
              "value": [
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["FALSE"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["NULL"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["6"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["TRUE"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["\"firm_adjusted\""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["task", "type"]
                }
              },
              "value": [
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["c(\"trajectory\", \"gap\", \"placebo\")"]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["parameter_set", "task"]
                }
              },
              "value": [
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["parameter_set", "task"]
                }
              },
              "value": [
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["group", "task"]
                }
              },
              "value": [
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["NULL"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["...", "base_url", "fn", "model", "type"]
                }
              },
              "value": [
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["NULL"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["NULL"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["NULL"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["NULL"]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["...", "provider", "x"]
                }
              },
              "value": [
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["NULL"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["caption", "col.names", "digits", "x"]
                }
              },
              "value": [
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["NULL"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["NULL"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["NULL"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["parameter_set", "report", "report_args", "task", "verbose"]
                }
              },
              "value": [
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["ParameterSet$new()"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["FALSE"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["list()"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["getOption(\"eventstudy.verbose\", TRUE)"]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["n_placebo", "task"]
                }
              },
              "value": [
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["NULL"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["abnormal_return", "alpha", "dgp_params", "estimation_window_length", "event_window", "n_events", "n_simulations", "return_model", "seed", "test_statistic"]
                }
              },
              "value": [
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["0"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["0.05"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["list()"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["120"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["c(-5, 5)"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["20"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["1000"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["MarketModel$new()"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["NULL"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["\"CSectT\""]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["base_family", "base_size"]
                }
              },
              "value": [
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["\"\""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["11"]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["...", "stat_name", "type", "x"]
                }
              },
              "value": [
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["\"CSectT\""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["c(\"ar\", \"car\", \"aar\", \"model\")"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["min_estimation_obs", "parameter_set", "task", "verbose"]
                }
              },
              "value": [
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["30"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["NULL"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": [""]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["getOption(\"eventstudy.verbose\", TRUE)"]
                }
              ]
            }
          ]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["ARTTest", "AnthropicProvider", "BHARModel", "BHARTTest", "BMPTest", "CARTTest", "CSectTTest", "CalendarTimePortfolioTest", "Carhart4FactorModel", "ComparisonPeriodMeanAdjustedModel", "CustomProvider", "DCCGARCHModel", "EventStudyTask", "FamaFrench3FactorModel", "FamaFrench5FactorModel", "GARCHModel", "GeneralizedSignTest", "IntradayEventStudyTask", "KolariPynnonenTest", "LinearFactorModel", "LogReturn", "MarketAdjustedModel", "MarketModel", "ModelBase", "MultiEventStatisticsSet", "OpenAICompatProvider", "PanelEventStudyTask", "ParameterSet", "PatellZTest", "ProviderBase", "RankTest", "ReturnCalculation", "RollingWindowModel", "SignTest", "SimpleReturn", "SingleEventStatisticsSet", "StatisticsSetBase", "SyntheticControlTask", "TestStatisticBase", "VolatilityModel", "VolumeModel"]
        },
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["es_colours"]
            }
          },
          "value": [
            {
              "type": "character",
              "attributes": {},
              "value": ["character"]
            }
          ]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["S3method(flag_robustness,EventStudyTask)", "S3method(flag_robustness,default)", "S3method(flag_robustness,es_diagnostics)", "S3method(format,Advice)", "S3method(format,EventStudySummary)", "S3method(format,es_advice)", "S3method(format,es_cross_sectional)", "S3method(format,es_diagnostics)", "S3method(format,es_simulation)", "S3method(generics::tidy,EventStudyTask)", "S3method(print,Advice)", "S3method(print,EventStudySummary)", "S3method(print,es_advice)", "S3method(print,es_cross_sectional)", "S3method(print,es_diagnostics)", "S3method(print,es_simulation)", "S3method(recommend_stat,EventStudyTask)", "S3method(recommend_stat,default)", "S3method(recommend_stat,es_diagnostics)"]
        }
      ]
    }

