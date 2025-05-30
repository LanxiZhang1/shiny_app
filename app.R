#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(data.table)
library(shinyWidgets)
library(plotly)
library(forecast)
library(rugarch)
library(ellipse)
library(rgl)


load("new_updated.RData")

merge_predictions <- function(arima_df, ewma_df, garch_df, hav_df) {
  
  # arima (rv)
  base_df <- arima_df %>%
    dplyr::select(stock_id, time_id, bucket30, rv, arima) %>%
    rename(ARIMA = arima)
  
  # EWMA
  base_df <- base_df %>%
    left_join(
      ewma_df %>% 
        filter(bucket30 > 16) %>%
          dplyr::select(stock_id, time_id, bucket30, ewma_forecast) %>%
          rename(EWMA = ewma_forecast),
        by = c("stock_id", "time_id", "bucket30")
    )
  
  # GARCH
  base_df <- base_df %>%
    left_join(
      garch_df %>%
        filter(bucket30 %in% c(17, 18, 19, 20)) %>%   # only keep these four bucket
        dplyr::select(stock_id, time_id, bucket30, garch_forecast) %>%
        rename(GARCH = garch_forecast),
      by = c("stock_id", "time_id", "bucket30")
    )
  
  # HAV RV
  base_df <- base_df %>%
    left_join(
      hav_df %>%
        dplyr::select(stock_id, time_id, bucket30, hav_pred) %>%
        rename(HAV = hav_pred),
      by = c("stock_id", "time_id", "bucket30")
    )
  
  return(base_df)
}


# ARMA(1,1)
forecast_arma <- function(rv_vec, h = 4) {
  rv_vec <- as.numeric(rv_vec)
  rv_vec <- rv_vec[is.finite(rv_vec)]     
  
  if (length(rv_vec) < 3 || sd(rv_vec) == 0) {
    const_fc <- if (length(rv_vec) > 0) rv_vec[length(rv_vec)] else NA_real_
    return(rep(const_fc, h))
  }
  
  out <- tryCatch({
    fit <- forecast::Arima(rv_vec,
                           order = c(1, 0, 1),
                           include.mean = TRUE,
                           method = "ML")
    as.numeric(forecast::forecast(fit, h = h)$mean)
  }, error = function(e) rep(NA_real_, h))
  
  out
}

# EWMA
forecast_ewma <- function(rv_vec, h = 4, lambda = 0.94) {
  rv_vec <- as.numeric(rv_vec)
  rv_vec <- rv_vec[!is.na(rv_vec)]
  
  if (length(rv_vec) == 0)
    return(rep(NA_real_, h))
  
  #σ_T^2
  sigma2 <- rv_vec[1]^2
  for (i in 2:length(rv_vec)) {
    sigma2 <- lambda * sigma2 + (1 - lambda) * rv_vec[i]^2
  }
  
  #σ_{T+1|T}^2 = σ_{T+2|T}^2 = … = σ_T^2
  rep(sqrt(sigma2), h)
}

forecast_garch <- function(rv_vec, h = 4) {
  rv_vec <- as.numeric(rv_vec)
  rv_vec <- rv_vec[is.finite(rv_vec)]      
  
  
  if (length(rv_vec) < 10 || sd(rv_vec) == 0)
    return(rep(NA_real_, h))
  
  
  spec <- rugarch::ugarchspec(
    variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
    mean.model     = list(armaOrder = c(1, 1), include.mean = TRUE),
    distribution.model = "norm"
  )
  
  
  fit <- tryCatch(
    rugarch::ugarchfit(spec, data = rv_vec, solver = "hybrid"),
    error = function(e) NULL
  )
  if (is.null(fit))
    return(rep(NA_real_, h))
  
  
  fc <- rugarch::ugarchforecast(fit, n.ahead = h)
  as.numeric(rugarch::sigma(fc))
}


# HAV/HAR
forecast_hav <- function(rv_vec, h = 4) {
  #vol_t = β0 + βd * vol_{t-1} + βw * mean(vol_{t-5:t-1})
  rv_vec <- as.numeric(rv_vec)
  if (length(rv_vec) < 6 || all(is.na(rv_vec))) {
    return(rep(NA_real_, h))
  }
  
  vol          <- rv_vec
  vol_1        <- c(NA, head(vol, -1))
  mean_vol_5   <- zoo::rollapply(vol, 5, mean, align = "right", fill = NA)
  
  df <- data.frame(vol, vol_1, mean_vol_5)
  train_df <- df[complete.cases(df), ]   
  
  if (nrow(train_df) < 6) {             
    return(rep(NA_real_, h))
  }
  
  
  model <- lm(vol ~ vol_1 + mean_vol_5, data = train_df)
  
  
  preds <- numeric(h)
  vol_future <- vol                     
  
  for (i in seq_len(h)) {
    vol_1_next      <- tail(vol_future, 1)
    mean_vol_5_next <- mean(tail(vol_future, 5))
    
    preds[i] <- pmax(1e-6, predict(model, newdata = data.frame(
      vol_1 = vol_1_next,
      mean_vol_5 = mean_vol_5_next
    )))
    
    vol_future <- c(vol_future, preds[i])  
  }
  
  preds
}


# change GARCH order
garch_forecast_list_corrected <- garch_forecast_list[c(2,3,1)]

combined_df_list <- lapply(1:3, function(i) {
  merge_predictions(
    arima_predictions_list[[i]],
    ewma_final_predictions_list[[i]],
    garch_forecast_list_corrected[[i]],
    hav_predictions_list[[i]]
  )
})

final_df <- bind_rows(combined_df_list)

# UI
ui <- dashboardPage(
  dashboardHeader(
    # need a shorter name :/
    title = "Stock Clustering Analysis"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("info-circle")),
      menuItem("Cluster Feature Distributions", tabName = "query", icon = icon("search")),
      menuItem("Model Comparison", tabName = "metrics", icon = icon("table")),
      menuItem("Volatility", tabName = "visual", icon = icon("chart-line")),
      menuItem("New Data Upload", icon = icon("upload"), startExpanded = TRUE,
               menuSubItem("Classification of projections", tabName = "predict_cluster"),
               menuSubItem("Volatility of projections", tabName = "volatility_forecast")
      )
      
    ),
    hr(),
    box(
      title = "📢 Observations",
      status = "danger",
      solidHeader = TRUE,
      width = 12,
      uiOutput("news_feed")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "query",
        fluidRow(
          box(width = 12, title = "Cluster Feature Distributions", status = "warning",solidHeader = TRUE,
              shinyWidgets::checkboxGroupButtons(
                inputId = "selected_features",
                label = "Select Features",
                choices = c("rv_mean", "rv_sd", "rv_skew", "rv_kurt", "wap_range", "depth_avg"),
                selected = c("rv_mean", "rv_sd", "rv_skew", "rv_kurt", "wap_range", "depth_avg"),
                status = "primary",
                checkIcon = list(yes = icon("ok", lib = "glyphicon"))
              ),
              shinyWidgets::checkboxGroupButtons(
                inputId = "selected_clusters",
                label = "Select Clusters",
                choices = sort(unique(stock_feat$cluster)),
                selected = sort(unique(stock_feat$cluster)),
                status = "primary",
                checkIcon = list(yes = icon("ok", lib = "glyphicon"))
              ),
              plotlyOutput("feature_boxplot", width = "100%", height = "600px")
          )
        )
      ),
      
      tabItem(
        tabName = "metrics", 
        fluidRow(
          box(
            width = 12, title = "Model Comparison", status = "warning", solidHeader = TRUE,
            selectInput("mc_metric", "Select Metric", choices = c("RMSE", "QLIKE", "U2")),
            selectInput("mc_cluster", "Select Cluster", choices = NULL),
            shinyWidgets::checkboxGroupButtons(
              inputId = "mc_models",
              label = "Select Models",
              choices = c("ARIMA", "EWMA", "GARCH", "HAV"),
              selected = c("ARIMA", "EWMA", "GARCH", "HAV"),
              status = "primary",
              checkIcon = list(yes = icon("ok", lib = "glyphicon"))
            ),
            DT::dataTableOutput("model_comparison_cluster_table"),
            plotOutput("model_comparison_boxplot", height = "400px")
          )
        )
      ),
      tabItem(
        tabName = "visual",
        fluidRow(
          box(
            width = 12, title = "Actual Volatility VS Model Predictions", status = "warning", solidHeader = TRUE,
            selectInput("stock_id_select", "Select Stock ID", choices = unique(final_df$stock_id)),
            selectInput("time_id_select", "Select time ID", choices = sort(unique(final_df$time_id))),
            plotOutput("volatility_plot", height = "450px")
          )
        )
      ),
      tabItem(
        tabName = "predict_cluster",
        fluidRow(
          box(
            width = 12, status = "primary", solidHeader = TRUE, title = "Input feature prediction categories",
            numericInput("rv_mean", "RV Mean", value = 0.0015),
            numericInput("rv_sd", "RV SD", value = 0.0012),
            numericInput("rv_skew", "RV Skewness", value = 1.49),
            numericInput("rv_kurt", "RV Kurtosis", value = 3.66),
            numericInput("wap_range", "WAP Range", value = 0.05),
            numericInput("depth_avg", "Depth Average", value = 213.59),
            actionButton("predict_cluster_btn", "Classification of projections", icon = icon("play")),
            verbatimTextOutput("cluster_prediction")
          ),
          box(
            width = 12, status = "info", solidHeader = TRUE,
            title = "Cluster Prediction Visualization",
            plotlyOutput("cluster_prediction_plot", height = "600px")
          )
          
        )
      ),
      
      
      tabItem(
        tabName = "volatility_forecast",
        fluidRow(
          box(
            width = 12, status = "info", solidHeader = TRUE, 
            title = "Enter the first 16 volatilities for forecasting",
            
            dropdownButton(
              label = "Select Stock and Time ID",
              status = "primary",
              circle = FALSE,
              icon = icon("sliders-h"),
              
              selectInput("stock_id_select", "Select Stock ID", choices = unique(final_df$stock_id)),
              selectInput("time_id_select", "Select Time ID", choices = sort(unique(final_df$time_id))),
              
              actionButton("confirm_selection", "Confirm Selection", icon = icon("check"))
            ),
            
            br(),
            
            lapply(1:16, function(i) {
              numericInput(paste0("rv_", i), paste("Volatility RV", i), value = NA)
            }),
            
            actionButton("predict_volatility_btn", "Forecasting subsequent volatility", icon = icon("play")),
            DTOutput("volatility_predictions")
          ),
          box(
            width = 12, status = "info", solidHeader = TRUE,
            title = "Forecast Results",
            plotlyOutput("rv_forecast_plot_2")
          )
          
        )
      ),
      tabItem(
        tabName = "intro",
        fluidRow(
          box(
            width = 12, status = "primary", solidHeader = TRUE,
            title = "Welcome",
            includeMarkdown("intro.md")
          )
        )
      )
      
    )
  )
)

# Server
server <- function(input, output, session) {
  
  observeEvent(input$confirm_selection, {
    req(input$stock_id_select, input$time_id_select)
    
    selected_data <- final_df %>%
      filter(stock_id == input$stock_id_select,
             time_id == input$time_id_select) %>%
      arrange(bucket30) %>%
      pull(rv)
    
    values_to_update <- if(length(selected_data) >= 16) selected_data[1:16] else rep(NA, 16)
    
    for (i in 1:16) {
      updateNumericInput(session, paste0("rv_", i), value = values_to_update[i])
    }
  })
  
  
  observeEvent(input$predict_volatility_btn, {
    rv_vec <- sapply(1:16, function(i) input[[paste0("rv_", i)]])
    
    arma_pred  <- forecast_arma(rv_vec, h = 4)
    ewma_pred  <- forecast_ewma(rv_vec, h = 4)
    garch_pred <- forecast_garch(rv_vec, h = 4)
    hav_pred   <- forecast_hav(rv_vec, h = 4)
    
    preds <- data.frame(
      s_ahead = 1:4,
      ARMA  = arma_pred,
      EWMA  = ewma_pred,
      GARCH = garch_pred,
      HAV   = hav_pred,
      check.names = FALSE
    )
    
    output$volatility_predictions <- renderDT({
      datatable(preds, rownames = FALSE, selection = 'none', options = list(dom = 't'))
    })
    
    output$rv_forecast_plot_2 <- renderPlotly({
      
      df_plot <- data.frame(
        Time = 1:20,
        RV = c(rv_vec, rep(NA, 4)),
        ARMA = c(rep(NA, 16), arma_pred),
        EWMA = c(rep(NA, 16), ewma_pred),
        GARCH = c(rep(NA, 16), garch_pred),
        HAV = c(rep(NA, 16), hav_pred)
      )
      
      plot_ly(df_plot, x = ~Time) %>%
        add_lines(y = ~RV, name = "Original RV (1-16)", line = list(width = 2)) %>%
        add_lines(y = ~ARMA, name = "ARMA Forecast", line = list(dash = 'dash')) %>%
        add_lines(y = ~EWMA, name = "EWMA Forecast", line = list(dash = 'dash')) %>%
        add_lines(y = ~GARCH, name = "GARCH Forecast", line = list(dash = 'dash')) %>%
        add_lines(y = ~HAV, name = "HAV Forecast", line = list(dash = 'dash')) %>%
        layout(
          xaxis = list(title = "Time (bucket)"),
          yaxis = list(title = "RV"),
          legend = list(orientation = "h", y = -0.2)
        )
    })
  })
  
  # Load the classification model
  trained_kmeans <- reactiveVal(list(
    centers = attr(res$mat, "scaled:center"),
    scales  = attr(res$mat, "scaled:scale"),
    kmeans_model = kmeans(res$mat, centers = length(unique(res$stock_feat$cluster)), nstart = 50)
  ))
  
  rv_values <- reactiveValues(mat_new = NULL, pred_cluster = NULL)
  
  # Classification prediction function implementation
  observeEvent(input$predict_cluster_btn, {
    new_features <- c(input$rv_mean, input$rv_sd, input$rv_skew,
                      input$rv_kurt, input$wap_range, input$depth_avg)
    
    rv_values$mat_new <- scale(matrix(new_features, nrow = 1),
                               center = trained_kmeans()$centers,
                               scale = trained_kmeans()$scales)
    
    rv_values$pred_cluster <- apply(rv_values$mat_new, 1, function(x) {
      dists <- apply(trained_kmeans()$kmeans_model$centers, 1, function(center) sum((x - center)^2))
      which.min(dists)
    })
    
    output$cluster_prediction <- renderPrint({
      paste("The predicted categories based on the input features are:", rv_values$pred_cluster)
    })
  })
  
  output$cluster_prediction_plot <- renderPlotly({
    req(rv_values$mat_new)
    
    centers_df <- as.data.frame(trained_kmeans()$kmeans_model$centers)
    centers_df$cluster <- factor(1:nrow(centers_df))
    
    new_point_df <- as.data.frame(rv_values$mat_new)
    colnames(new_point_df) <- colnames(centers_df)[1:ncol(new_point_df)]
    new_point_df$cluster <- factor("Predicted")
    
    original_points_df <- as.data.frame(res$mat)
    original_points_df$cluster <- factor(trained_kmeans()$kmeans_model$cluster)
    
    cluster_colors <- RColorBrewer::brewer.pal(n = length(unique(original_points_df$cluster)), "Set1")
    
    plot_ly() %>%
      add_markers(
        data = original_points_df,
        x = ~rv_mean, y = ~rv_sd, z = ~wap_range,
        color = ~cluster, colors = cluster_colors,
        size = I(5), opacity = 3,
        marker = list(symbol = 'circle'),
        name = ~paste("Cluster", cluster),
        legendgroup = ~cluster
      ) %>%
      add_markers(
        data = centers_df,
        x = ~rv_mean, y = ~rv_sd, z = ~wap_range,
        color = ~cluster, colors = cluster_colors,
        marker = list(symbol = 'x', size = 5, opacity = 1),
        name = ~paste("Cluster Center", cluster),
        legendgroup = ~cluster,
        showlegend = TRUE
      ) %>%
      add_markers(
        data = new_point_df,
        x = ~rv_mean, y = ~rv_sd, z = ~wap_range,
        marker = list(symbol = 'diamond', color = 'purple', size = 4),
        name = "Predicted Point",
        legendgroup = "Predicted"
      ) %>%
      layout(
        scene = list(
          xaxis = list(title = "RV Mean (scaled)"),
          yaxis = list(title = "RV SD (scaled)"),
          zaxis = list(title = "WAP Range (scaled)")
        ),
        legend = list(orientation = "h", y = -0.1)
      )
  })
  
  
  
  
  # feature distributions
  output$feature_boxplot <- renderPlotly({
    req(input$selected_features, input$selected_clusters)
    df <- stock_feat %>%
      filter(cluster %in% input$selected_clusters) %>%
      tidyr::pivot_longer(
        cols = all_of(input$selected_features),
        names_to = "feature", values_to = "value"
      )
    p <- ggplot(df, aes(x = factor(cluster), y = value, fill = factor(cluster))) +
      geom_boxplot() +
      facet_wrap(~feature, scales = "free", ncol = 2) +
      theme_minimal() +
      labs(x = "Cluster", y = "Value")
    ggplotly(p)
  })
  
  ### Just non-interactive boxplots
  # output$feature_boxplot <- renderPlot({
  #   library(tidyr)
  #   library(dplyr)
  #   stock_feat %>%
  #     tidyr::pivot_longer(
  #       cols = c(rv_mean, rv_sd, rv_skew, rv_kurt, wap_range, depth_avg),
  #       names_to = "feature", values_to = "value"
  #     ) %>%
  #     ggplot(aes(x = cluster, y = value, fill = cluster)) +
  #     geom_boxplot() +
  #     facet_wrap(~feature, scales = "free", ncol = 3) +
  #     theme_minimal() +
  #     labs(title = "Cluster-wise Feature Distributions", x = "Cluster", y = "Value") +
  #     theme(legend.position = "none")
  # })
  
  # Update cluster choices based on stock_feat
  observe({
    updateSelectInput(session, "mc_cluster", choices = sort(unique(stock_feat$cluster)))
  })
  
  output$model_comparison_cluster_table <- DT::renderDataTable({
    req(input$mc_metric, input$mc_cluster, input$mc_models)
    metric <- toupper(input$mc_metric)
    cluster_sel <- as.character(input$mc_cluster)
    models <- input$mc_models
    values <- sapply(models, function(m) {
      dfname <- paste0(tolower(m), "_", tolower(metric), "_df")
      if (exists(dfname)) {
        df <- get(dfname)
        val <- NA
        if ("cluster_id" %in% names(df) && metric %in% names(df)) {
          val <- median(df[[metric]][df$cluster_id == cluster_sel], na.rm = TRUE)
        }
        return(val)
      }
      NA
    })
    tab <- data.frame(
      Model = models,
      Median = as.numeric(values)
    )
    ## remove highlights & length menu
    DT::datatable(tab, rownames = FALSE, selection = 'none', options=list(dom='t'))
  })
  ## create boxplot (show up to 95th percentile)
  output$model_comparison_boxplot <- renderPlot({
    req(input$mc_metric, input$mc_cluster, input$mc_models)
    metric <- toupper(input$mc_metric)
    cluster_sel <- as.character(input$mc_cluster)
    models <- input$mc_models
    
    data_list <- lapply(models, function(m) {
      dfname <- paste0(tolower(m), "_", tolower(metric), "_df")
      if (exists(dfname)) {
        df <- get(dfname)
        if ("cluster_id" %in% names(df) && metric %in% names(df)) {
          sub <- df[df$cluster_id == cluster_sel, c("stock_id", "cluster_id", metric)]
          sub$model <- m
          names(sub)[which(names(sub) == metric)] <- "value"
          return(sub)
        }
      }
      NULL
    })
    df_all <- do.call(rbind, data_list)
    
    # Calculate a reasonable upper limit (e.g., 95th percentile)
    y_max <- quantile(df_all$value, 0.95, na.rm = TRUE)
    
    library(ggplot2)
    ggplot(df_all, aes(x = model, y = value, fill = model)) +
      geom_boxplot(width = 0.6, outlier.size = 1.5) +
      coord_cartesian(ylim = c(NA, y_max)) +   # <--- add this!
      labs(
        title = paste("Distribution of", metric, "by Model | Cluster:", cluster_sel),
        x = "Model",
        y = metric
      ) +
      theme_minimal()
  })
  
  # select stock_id
  observe({
    sorted_stock_ids <- sort(unique(final_df$stock_id))
    updateSelectInput(session, "stock_id_select",
                      choices = sorted_stock_ids,
                      selected = sorted_stock_ids[1])
  })
  
  # select stock_id
  observeEvent(input$stock_id_select, {
    available_time_ids <- final_df %>%
      filter(stock_id == input$stock_id_select) %>%
      pull(time_id) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "time_id_select",
                      choices = available_time_ids,
                      selected = available_time_ids[1])
  })
  
  # Volatility plot
  output$volatility_plot <- renderPlot({
    req(input$stock_id_select, input$time_id_select)
    
    plot_data <- final_df %>%
      filter(stock_id == input$stock_id_select,
             time_id == input$time_id_select) %>%
      arrange(bucket30)
    
    # rv
    plot(plot_data$bucket30, plot_data$rv,
         type = "l", col = "black", lwd = 2,
         ylim = range(plot_data[, c("rv", "ARIMA", "EWMA", "GARCH", "HAV")], na.rm = TRUE),
         xlab = "Bucket30",
         ylab = "Volatility",
         main = paste("Volatility Comparison for Stock", input$stock_id_select, 
                      "at Time ID", input$time_id_select))
    
    # model prediction
    lines(plot_data$bucket30, plot_data$ARIMA, col = "blue", lwd = 2, lty = 2)
    lines(plot_data$bucket30, plot_data$EWMA, col = "red", lwd = 2, lty = 2)
    lines(plot_data$bucket30, plot_data$GARCH, col = "green", lwd = 2, lty = 2)
    lines(plot_data$bucket30, plot_data$HAV, col = "purple", lwd = 2, lty = 2)
    
    # legend
    legend("topright",
           legend = c("Actual RV", "ARIMA", "EWMA", "GARCH", "HAV"),
           col = c("black", "blue", "red", "green", "purple"),
           lty = c(1,2,2,2,2),
           lwd = 2, cex = 0.8)
  })
  
  
  output$news_feed <- renderUI({
    news <- c(
      "🔥 Group 6",
      "📈 Clustering based on stock similarities",
      "⚠️ Coomparing models to find best model to use",
      "🔔 T-test for stock vs cluster",
      "💡 All p-values were above 5%",
      "📉 No significant difference between stock performance and cluster performance",
      "🌍 Can construct one model per cluster instead of one model per stock"
    )
    
    HTML(sprintf('
    <div id="scroll_box" style="height:250px; overflow:hidden; position:relative; background:#ffffff; color:#000000; font-size:14px; line-height:1.6; padding-left:10px;">
      <div id="scroll_content" style="position:absolute;">
        %s
      </div>
    </div>
    <script>
      (function(){
        var box = document.getElementById("scroll_box");
        var content = document.getElementById("scroll_content");
        var pos = box.offsetHeight;
        setInterval(function(){
          pos--;
          if(pos <= -content.offsetHeight){
            pos = box.offsetHeight;
          }
           // 30 milliseconds
          content.style.top = pos + "px";
        }, 30);  
      })();
    </script>
    ',
                 paste(
                   sprintf(
                     '<p style="margin:0 0 8px 0;"><i class="fa fa-circle" style="color:#17a2b8; margin-right:6px;"></i>%s</p>',
                     news
                   ),
                   collapse = ""
                 )
    ))
  })
}

shinyApp(ui, server)