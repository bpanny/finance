# Finance: S\&P 500 PCA & Clustering Analysis

This is a financial time-series analysis on S\&P 500 stock data. It includes scripts to scrape historical closing prices using `tidyquant`, preprocess the data, and apply Principal Component Analysis (PCA) and Hierarchical Clustering to group stocks based on their relative performance patterns over time.

## Project Structure

### 1\. Data Acquisition

  * **Script:** `get_sp500_closing_price_tidyquant.R`
  * **Purpose:** Downloads and prepares the raw data.
  * **Key Operations:**
      * Fetches the current list of S\&P 500 symbols using `tidyquant`.
      * Retrieves daily stock prices for the last 2 years (dynamic date range based on `Sys.Date()`).
      * Filters out stocks that do not have data for every single trading day in the period (ensures complete datasets).
      * **Outputs:** Generates two CSV files used for analysis:
          * `sp500_stock_close_long_format.csv`
          * `sp500_stock_close_wide_format.csv`

### 2\. Analysis & Visualization

  * **Script:** `example_pca_hclust_sp500_relative_close.R`
  * **Purpose:** Analyzes the downloaded data using statistical learning techniques.
  * **Key Operations:**
      * **Preprocessing:** Calculates the "Relative Closing Price" for each stock (percentage change relative to the first day of the dataset).
      * **Visualization:** Generates line plots, boxplots, and ribbon plots to visualize market trends and distributions.
      * **PCA:** Performs Principal Component Analysis to reduce dimensionality and identify days that contribute most to market variance.
      * **Clustering:** Applies Hierarchical Clustering (Ward's method) on the PCA scores to group stocks into 5-8 distinct clusters based on price movement similarities.

### 3\. Project Configuration

  * **`finance.Rproj`**: RStudio project file. Open this to ensure your working directory is correctly set.
  * **`.gitignore`**: Specifies intentionally untracked files (e.g., user workspace configurations, history).

## Dependencies

To run these scripts, you will need R installed along with the following packages:

```r
install.packages(c("tidyverse", "tidyquant", "caret", "factoextra", "corrplot", "ggthemes"))
```

## How to Run

1.  **Step 1: Get the Data**
    Run `get_sp500_closing_price_tidyquant.R` first. This will download the most recent S\&P 500 data and save the necessary CSV files to your working directory.
    *(Note: This requires an active internet connection.)*

2.  **Step 2: Analyze**
    Run `example_pca_hclust_sp500_relative_close.R`. This script reads the CSVs generated in Step 1.

      * It will produce various plots in your R plot viewer showing stock performance, PCA scree plots, and cluster visualizations.
      * It creates a variable `symbol_clusters` which assigns a cluster ID to each stock symbol.

## Methodology Details

  * **Relative Closing Price:** The analysis does not use raw prices. Instead, it normalizes all stocks to start at 0 on Day 1. The formula used is:
    $$\text{Relative Close} = \frac{\text{Close Price} - \text{First Day Close}}{\text{First Day Close}}$$
  * **Dimensionality Reduction:** The script converts the time-series data into a "wide" format where every day is a column, then uses PCA to compress these daily movements into principal components.
  * **Clustering:** Stocks are clustered based on their PCA scores (specifically the first 16 PCs), allowing for the identification of stocks that behave similarly (e.g., Tech stocks might cluster together vs. Utilities).
