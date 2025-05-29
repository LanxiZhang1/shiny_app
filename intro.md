# Stock Clustering & Volatility Dashboard

This Shiny dashboard is an **end-to-end interactive tool for “explore → evaluate → forecast” workflows** in high-frequency trading and quantitative research.  
By combining clustering analysis, four volatility-forecasting models (ARIMA, EWMA, GARCH, HAV-RV) and rich visual components, users can quickly explore stock features, compare model performance, and generate short-term volatility predictions in real time.

---

## Core Features & Page Layout

| Sidebar Entry | Main Components | What You Can Do |
|---------------|----------------|-----------------|
| **Cluster Feature Distributions** | • Multi-select features & clusters  <br>• Plotly faceted boxplots | Compare distributions of `rv_mean`, `rv_sd`, `wap_range`, etc. across clusters to verify clustering logic. |
| **Model Comparison** | • Metric selector (RMSE / QLIKE / U2)  <br>• Cluster filter  <br>• Multi-model toggle  <br>• Data table + boxplot | For a given cluster, inspect error distributions and medians across four models; spot outliers with boxplots and identify the most stable model with the table. |
| **Volatility** | • Stock ID & Time ID dropdowns  <br>• Actual RV vs. 4-model prediction lines | Over a 20 × 30 s bucket window, visually assess how closely each model tracks realised volatility. |
| **Classification of Projections** | • Six numeric inputs  <br>• Predict button  <br>• Text result + 3-D Plotly scatter | Feed new features into the trained **k-means** model, get an instant cluster label, and view the point relative to original data and cluster centres. |
| **Volatility of Projections** | • 16 historic RV inputs  <br>• One-click ARMA/EWMA/GARCH/HAV-RV 4-step forecast  <br>• Table + interactive line chart | Import (or type) the latest 16 RV values, receive four model forecasts for the next four buckets, and compare trajectories. |
| **📢 Observations Ticker** | • Pure front-end JS news-style scroller | Loop task status, team reminders or alerts in the sidebar; easily customise text or hook up to a crawler. |

---

## Technical Highlights

### Data Integration
`merge_predictions()` stitches the four model forecasts to the original RV values, producing a global `final_df`.

### Four Forecast Engines
* **`forecast_arma()`** — ARMA (1, 1)  
* **`forecast_ewma()`** — Classic λ = 0.94 EWMA  
* **`forecast_garch()`** — sGARCH (1, 1) with ARMA (1, 1) mean  
* **`forecast_hav()`** — HAV-RV (daily-weekly hybrid regression)

### Clustering Module
Training data `res$mat` are standardised (mean–variance) and clustered via *k-means*.  
Prediction flow: input → `scale()` → compute Euclidean distance to each centre → choose nearest cluster.

### Fully Interactive Visualisation
Plotly 3-D scatters, faceted boxplots and line charts support zoom & hover; themes controlled consistently through **shinydashboard** and **shinyWidgets**.

---

## Getting Started

1. **Prepare data**: ensure `1.RData` contains  
   * raw feature matrix `res$mat`,  
   * cluster assignments `stock_feat`,  
   * all model-metric data frames.  
2. **Run**: click **Run App** in RStudio or call `shiny::runApp()`.  
3. **Explore**: step through the sidebar pages; default inputs already showcase the workflow.

---

## What You Gain

* **Rapid validation** – see whether clustering cleanly separates volatility behaviour; view per-cluster error landscapes.  
* **Instant forecasting** – enter the latest RV sequence and get multi-model rolling predictions with visual comparison.  
* **Presentation-ready** – clean UI and dynamic charts make insights clear for classrooms, clients or teammates.

Have fun and good alpha hunting! 🧠📈