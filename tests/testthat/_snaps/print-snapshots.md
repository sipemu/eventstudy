# print.EventStudySummary snapshot

    Code
      print(x)
    Output
      Event Study Summary
      ===================
      Events:  3 
      Groups:  treated, control 
      Symbols: AAA, BBB 
      
      Model Statistics:
         AAA : alpha= 0.001234  beta= 1.2346  sigma= 0.009876  R2= 0.8765 
         BBB : NOT FITTED

# print.es_diagnostics snapshot (fitted)

    Code
      print(x)
    Output
      Event Study Diagnostics
      =======================
      Events total:    5 
      Events shown:    5 (full detail)
      Events valid:    5 
      
      Estimation window (medians across shown events):
        R-squared:     0.81 
        Shapiro-Wilk p: 0.42 
        DW statistic:  1.98 
      
      Event window (cross-sectional):
        CAR IQR:        0.023457 
        Overlap pairs:  4 

# print.es_diagnostics snapshot (degenerate WARN branch)

    Code
      print(x)
    Output
      Event Study Diagnostics
      =======================
      Events total:    3 
      Events shown:    3 (full detail)
      Events summarized: 2 (aggregate summary only)
      [WARN] 2 degenerate event(s) in shown set (is_fitted=FALSE)
      
      Estimation window (medians across shown events):
        R-squared:     0.575 
        Shapiro-Wilk p: 0.15 
        DW statistic:  2 
      
      Event window (cross-sectional):
        CAR IQR:        NA 
        Overlap pairs:  NA 

# print.es_simulation snapshot

    Code
      print(x)
    Output
      Event Study Simulation
        N events:        50 
        Event window:   [ -5 , 5 ]
        Abnormal return: 0.01 
        Test statistic:  car_t 
        Alpha:           0.05 
        N simulations:   1000 
        Power (day 0):   0.8374 

# print.es_cross_sectional snapshot

    Code
      print(x)
    Output
      Cross-Sectional Regression of CARs
      ===================================
      N: 42 
      R-squared: 0.2346 
      Adj. R-squared: 0.1988 
      
      Coefficients:
                   estimate std_error   t_value  p_value
      (Intercept)  0.001234  0.000543  2.273456 0.028765
      size        -0.023457  0.010988 -2.134567 0.039876

# print.Advice snapshot (with recommendations + guard drop)

    Code
      print(x)
    Output
      Event Study Advice
      ==================
      Source:         llm 
      Task type:      recommend_stat 
      Deterministic:  FALSE 
      Recommendations: 1 
      [GUARD] 2 recommendation(s) dropped as ungrounded.
      
      Interpretation:
        CARs are significantly positive around the event. 
      
      [1] Use BMP test
          Kind:    statistic 
          Effect:  more robust to variance inflation 
          Evidence:
            event_var_ratio = 3.2 (threshold 1.5, above)
      
      Caveats:
       - Small sample. 
       - Overlapping windows. 

# print.es_advice snapshot (zero rules)

    Code
      print(x)
    Output
      Offline Event Study Advice
      ==========================
      Source:           offline_kb 
      Deterministic:    TRUE 
      Rules matched:    0 
      
      (No rules fired on these diagnostics.)

# print.es_advice snapshot (>=1 rule)

    Code
      print(x)
    Output
      Offline Event Study Advice
      ==========================
      Source:           offline_kb 
      Deterministic:    TRUE 
      Rules matched:    1 
      
      [WARNING] R001  (citation: MacKinlay1997)
        Recommendation: Prefer BMP over plain t-test under variance inflation. 
      

