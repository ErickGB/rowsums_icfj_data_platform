# ***********************************************
# Load libraries ----
library(tidyverse)
library(tidyquant)
library(GGally)
library(ggthemes) 
library(plotly)
library(anomalize)
library(cowplot)
library(fs)
library(glue)
library(vtreat)  # categorical w
# ************************************************

# *********************************************************
# anomaly detection with Isolation Forest ----
# 1 indica anomalías
# menor que 0.5 indica observaciones normales
# Si todas las puntuaciones están cerca de 0,5 la muestra completa no parece tener anomalías claramente distintas

get_outliers <- function(data_tbl){
	localH2O = h2o.init()
	raw_cloud <-as.h2o(data_tbl, destination_frame="train_hex")
	
	n_trees <- 100
	isolation_forest_model <- h2o.isolationForest(
		stopping_rounds = 3, # stopping_metric = "AUC",
		stopping_tolerance = 1e-2,
		training_frame = raw_cloud,
		ntrees = n_trees, seed = 12345)
	
	predictions <- isolation_forest_model %>% 
		h2o.predict(newdata = raw_cloud)

	# ¿Cómo sabemos cuál es la media del número de splits? usando un umbral!.. aquellos que están dentro del 95% son normales
	quantile_frame <- h2o.quantile(predictions, probs = 0.95) 
	
	threshold <- as.numeric(quantile_frame[1])
	predictions_tbl <- predictions %>% as_tibble()
	#hist(predictions_tbl$predict)
	#View(predictions_tbl)
	
	data_tbl$predict <- as.numeric(predictions_tbl$predict)
	anomaly <- ifelse(data_tbl$predict  > threshold, 1, 0)
	# aquellos que están sobre del umbral ----
	#data_outliers_tbl <- data_tbl %>% 
	#	filter(predict > threshold) 
	#data_outliers_tbl$threshold <- threshold
	
	#h2o.shutdown()
	return(anomaly)
} 


# *********************************************************




treatment_categorical <- function(data, variable)	
{
	#Prepare a treatment plan for the dataframe
	treatment <- vtreat::designTreatmentsN(dframe = data,
	                                       varlist = variable, #c("organization_type"),
	                                       outcomename = 'target',
	                                       verbose = F)
	
	scoreFrame = treatment$scoreFrame
	
	#Which vars do we want to keep
	vars <- scoreFrame$varName[(scoreFrame$code %in% c("catN", "clean"))]
	
	#Apply the treatment plan
	treated_data = vtreat::prepare(data,
	                               treatmentplan = treatment,
	                               varRestriction = vars)
	
	treated_data <- treated_data %>% 
		select(1)
	
	return(treated_data)
}


delete_rows <- function(data, col, symbol, value)
{
	col_exp <- enquo(col)
	if(symbol == ">") {
		data <- data %>% 
			filter( (!! col_exp) > value)
	} 
	if(symbol == "<") {
		data <- data %>% 
			filter( (!! col_exp) < value)
	} 	
	if(symbol == "=") {
		data <- data %>% 
			filter( (!! col_exp) == value)
	} 
	return(data)
}


summary_contigency_table <- function(data, ..., col) {
	grouping_vars <- quos(...)
	col_var <- enquo(col) 
	
	ret <- data %>% 
		group_by(!!! grouping_vars) %>% 
		count(!! col_var) %>% 
		mutate(pct = round((n) / sum(n), digits = 2) ) %>% 
		ungroup()
	
	ret$Colname = rep(as.character(col_var)[2], times=nrow(ret))
	
	return(ret) 
}

summary_col_by_group <- function(data, ..., col, alpha = 0.05) {
	grouping_vars <- quos(...)
	col_var <- enquo(col) 
	
	ret <- data %>% 
		group_by(!!! grouping_vars) %>% 
		summarize(Count=n(), Total=sum(!! col_var, na.rm = TRUE), 
			      Min  = min(!! col_var, na.rm = TRUE), 
			      Max  = max(!! col_var, na.rm = TRUE), 
			      SD   = sd( !! col_var, na.rm = TRUE), 
			      Q1   = quantile(!! col_var, probs = c(0.25), na.rm = TRUE),
            Mean = mean(!! col_var, na.rm = TRUE), 
			      Median=median(!! col_var, na.rm = TRUE), 
			      Skewed = skewness(!! col_var, na.rm = TRUE), 
			      normality_test = ks.test(x=(!! col_var) ,y='pnorm')$p.value,
			      Q3   = quantile(!! col_var, probs = c(0.75), na.rm = TRUE),
			      IQR  = IQR(!! col_var, na.rm = TRUE),
			      OUT_LI  = (1.5 * IQR) - Mean, #Mean - (3 * SD), 
			      OUT_LS  = (1.5 * IQR) + Mean, #Mean + (3 * SD), 
			      COUNT_OUT = sum(ifelse( (!! col_var) > OUT_LS, 1, 0), na.rm = TRUE) + sum(ifelse( (!! col_var) < OUT_LI, 1, 0), na.rm = TRUE),
			      PER_COUNT = (COUNT_OUT / Count) * 100,
			      SE   = Mean/sqrt(Count), 
            t    = qt(p=(1 - (alpha/2)), df=(Count - 1)),
            LS   = Mean + (t  * SE), 
			      LI = Mean - (t  * SE)) %>% 
		ungroup() %>% 
		as.tibble() 

	ret$Colname = rep(as.character(col_var)[2], times=nrow(ret))
	
	return(ret) 
}

get_cor <- function(data, target, use = "pairwise.complete.obs",
                    fct_reorder = FALSE, fct_rev = FALSE) {
    
    feature_expr <- enquo(target)
    feature_name <- quo_name(feature_expr)
    
    data_cor <- data %>%
        mutate_if(is.character, as.factor) %>%
        mutate_if(is.factor, as.numeric) %>%
        cor(use = use) %>%
        as.tibble() %>%
        mutate(feature = names(.)) %>%
        select(feature, !! feature_expr) %>%
        filter(!(feature == feature_name)) %>%
        mutate_if(is.character, as_factor)
    
    if (fct_reorder) {
        data_cor <- data_cor %>% 
            mutate(feature = fct_reorder(feature, !! feature_expr)) %>%
            arrange(feature)
    }
    
    if (fct_rev) {
        data_cor <- data_cor %>% 
            mutate(feature = fct_rev(feature)) %>%
            arrange(feature)
    }
    
    return(data_cor)
    
}

independent_chiq_test <- function(data, col_outcome, col_predictor) {
	col_outcome_exp <- enquo(col_outcome)
	col_predictor_exp <- enquo(col_predictor)
	  # test for significance correlation between categorical featrues (target vs housetype_mode) 
		tbl <- data %>% 
				tabyl( (!! col_outcome_exp) , (!! col_predictor_exp) ) %>% 
			  as.matrix()
		tbl <- tbl[,2:ncol(tbl)] 
		# dimnames(tbl) <- list(default=  , houseType=c("block_of_flats", "specific_housing", "terraced_house", "NA_"))
		
		# chi-squared test: p-value here can be seen as a measure of correlation between these two variables.
		#                   if the variable are independent 
		# Ho: Null hypothesis, we assume uniform distribution (independent variables)
		# Hi: No uniform distribution (if they are dependent variable, are correlated)
		chi2 <- chisq.test(tbl, correct=F)
		return(chi2$p.value)
}

plot_staked_bar <- function(data, by) {
	by_exp = enquo(by)

g <- data %>% 
	gather("feature","value", 2:ncol(data))  %>% 
	count((!! by_exp), feature, value) %>% 
	mutate(value = tolower(value)) %>% 
	arrange(value, target, n) %>% 
	# filter(target == 1) %>% 
	ggplot(aes(x = reorder(value, desc(-n)), y = n, fill = as.factor(!! by_exp))) + # reorder(value, desc(-n))
	geom_bar(stat = "identity") + 
	coord_flip() + 
	theme_hc(bgcolor = "darkunica") + 
	theme(axis.text.y = element_text(colour = "gray")) + 
	scale_fill_hc("darkunica") + 
	ggtitle("Education grade") + 
	labs(x  = "", y = "", fill = "Target") + 
	facet_wrap(feature ~ ., ncol = 4, scales = "free_y")
	
	return(g)
}

plot_boxplot_base <- function(data, by, with_plotly = FALSE) {
	by_exp = enquo(by)

		g <- data %>% 
			gather("feature","value", 2:ncol(data))  %>% 
			filter(is.na(value) == FALSE) %>% 
			# filter(target == 1) %>% 
			ggplot(aes(x = as.factor(!! by_exp), y = value, fill = as.factor(!! by_exp))) + # reorder(value, desc(-n))
			   geom_boxplot() + 
		     facet_wrap(feature ~ ., ncol = 4, scales = "free_y") +
			   theme_hc() + 
			   scale_colour_tableau("colorblind10") + 
			   scale_fill_tableau("colorblind10") + 
			   labs(x  = "", y = "", fill = "Target") + 
			   #scale_fill_hc("darkunica") + 
			   #scale_colour_hc("darkunica") + 
			   theme(
			   	axis.text.y = element_text(colour = "gray"), 
			   	panel.grid.major = element_line(colour = "gray"),
			   	panel.grid.minor = element_line(colour="gray", linetype="dashed", size=0.2)
			   	) 
		if(with_plotly==TRUE)	
			g <- ggplotly(g)
		
		return(g)
}

plot_hist_facet <- function(data, fct_reorder = FALSE, fct_rev = FALSE, 
                            bins = 10, fill = palette_light()[[3]], color = "white", ncol = 5, scale = "free") {
    
    data_factored <- data %>%
        mutate_if(is.character, as.factor) %>%
        mutate_if(is.factor, as.numeric) %>%
        gather(key = key, value = value, factor_key = TRUE) 
    
    if (fct_reorder) {
        data_factored <- data_factored %>%
            mutate(key = as.character(key) %>% as.factor())
    }
    
    if (fct_rev) {
        data_factored <- data_factored %>%
            mutate(key = fct_rev(key))
    }
    
    g <- data_factored %>%
        ggplot(aes(x = value, group = key)) +
        geom_histogram(bins = bins, fill = fill, color = color) +
        facet_wrap(~ key, ncol = ncol, scale = scale) + 
        theme_tq()
    
    return(g)
    
}

# ggpairs: A lot of repetitive typing can be reduced 
plot_ggpairs <- function(data, color = NULL, density_alpha = 0.5) {
    color_expr <- enquo(color)
    
    if (rlang::quo_is_null(color_expr)) {
        
        g <- data %>%
            ggpairs(lower = "blank", cardinality_threshold = 60) 
        
    } else {
        
        color_name <- quo_name(color_expr)
        
        g <- data %>%
            ggpairs(mapping = aes_string(color = color_name), 
                    legend = 1,  lower = "blank", 
            	      cardinality_threshold = 60,
                    diag = list(continuous = wrap("densityDiag", 
                                                  alpha = density_alpha))) +
            theme(legend.position = "bottom")
    }
    
    return(g)
    
}


plot_cor <- function(data, target, fct_reorder = FALSE, fct_rev = FALSE, 
                     include_lbl = TRUE, lbl_precision = 2, lbl_position = "outward",
                     size = 2, line_size = 1, vert_size = 1, 
                     color_pos = palette_light()[[1]], color_neg = palette_light()[[2]]) {
    
    feature_expr <- enquo(target)
    feature_name <- quo_name(feature_expr)
    
    data_cor <- data %>%
        get_cor(!! feature_expr, fct_reorder = fct_reorder, fct_rev = fct_rev) %>%
        mutate(feature_name_text = round(!! feature_expr, lbl_precision)) %>%
        mutate(Correlation = case_when(
            (!! feature_expr) >= 0 ~ "Positive",
            TRUE                   ~ "Negative") %>% as.factor())
    
    g <- data_cor %>%
        ggplot(aes_string(x = feature_name, y = "feature", group = "feature")) +
        geom_point(aes(color = Correlation), size = size) +
        geom_segment(aes(xend = 0, yend = feature, color = Correlation), size = line_size) +
        geom_vline(xintercept = 0, color = palette_light()[[1]], size = vert_size) +
        expand_limits(x = c(-1, 1)) +
        theme_tq() +
        scale_color_manual(values = c(color_neg, color_pos)) 
    
    if (include_lbl) g <- g + geom_label(aes(label = feature_name_text), hjust = lbl_position)
    
    return(g)
    
}


# Visualize the H2O leaderboard to help with model selection
plot_h2o_leaderboard <- function(h2o_leaderboard, order_by = c("auc", "logloss"), 
                                 n_max = 20, size = 4, include_lbl = TRUE) {
    
    # Setup inputs
    order_by <- tolower(order_by[[1]])
    
    leaderboard_tbl <- h2o_leaderboard %>%
        as.tibble() %>%
        mutate(model_type = str_split(model_id, "_", simplify = T) %>% .[,1]) %>%
        rownames_to_column(var = "rowname") %>%
        mutate(model_id = paste0(rowname, ". ", as.character(model_id)) %>% as.factor())
    
    # Transformation
    if (order_by == "auc") {
        
        data_transformed_tbl <- leaderboard_tbl %>%
            slice(1:n_max) %>%
            mutate(
                model_id   = as_factor(model_id) %>% reorder(auc),
                model_type = as.factor(model_type)
            ) %>%
            gather(key = key, value = value, 
                   -c(model_id, model_type, rowname), factor_key = T)
        
    } else if (order_by == "logloss") {
        
        data_transformed_tbl <- leaderboard_tbl %>%
            slice(1:n_max) %>%
            mutate(
                model_id   = as_factor(model_id) %>% reorder(logloss) %>% fct_rev(),
                model_type = as.factor(model_type)
            ) %>%
            gather(key = key, value = value, -c(model_id, model_type, rowname), factor_key = T)
        
    } else {
        stop(paste0("order_by = '", order_by, "' is not a permitted option."))
    }
    
    # Visualization
    g <- data_transformed_tbl %>%
        ggplot(aes(value, model_id, color = model_type)) +
        geom_point(size = size) +
        facet_wrap(~ key, scales = "free_x") +
        theme_tq() +
        scale_color_tq() +
        labs(title = "Leaderboard Metrics",
             subtitle = paste0("Ordered by: ", toupper(order_by)),
             y = "Model Postion, Model ID", x = "")
    
    if (include_lbl) g <- g + geom_label(aes(label = round(value, 2), hjust = "inward"))
    
    return(g )
    
}


# Extracts and H2O model name by a position so can more easily use h2o.getModel()
extract_h2o_model_name_by_position <- function(h2o_leaderboard, n = 1) {
    
    model_name <- h2o_leaderboard %>%
        as.tibble() %>%
        slice(n) %>%
        pull(model_id) 
    
    #print(model_name)
    
    return(model_name)
    
}


plot_h2o_performance <- function(h2o_leaderboard, newdata, order_by = c("auc", "logloss"),
                                 max_models = 3, size = 1.5) {
    
    # Inputs
    
    leaderboard_tbl <- h2o_leaderboard %>%
        as.tibble() %>%
        slice(1:max_models)
    
    newdata_tbl <- newdata %>%
        as.tibble()
    
    order_by <- tolower(order_by[[1]])
    order_by_expr <- rlang::sym(order_by)
    
    h2o.no_progress()
    
    # 1. Model metrics
    
    get_model_performance_metrics <- function(model_id, test_tbl) {
        
        model_h2o <- h2o.getModel(model_id)
        perf_h2o  <- h2o.performance(model_h2o, newdata = as.h2o(test_tbl))
        
        perf_h2o %>%
            h2o.metric() %>%
            as.tibble() %>%
            select(threshold, tpr, fpr, precision, recall)
        
    }
    
    model_metrics_tbl <- leaderboard_tbl %>%
        mutate(metrics = map(model_id, get_model_performance_metrics, newdata_tbl)) %>%
        unnest() %>%
        mutate(
            model_id = as_factor(model_id) %>% 
                fct_reorder(!! order_by_expr, .desc = ifelse(order_by == "auc", TRUE, FALSE)),
            auc  = auc %>% 
                round(3) %>% 
                as.character() %>% 
                as_factor() %>% 
                fct_reorder(as.numeric(model_id)),
            logloss = logloss %>% 
                round(4) %>% 
                as.character() %>% 
                as_factor() %>% 
                fct_reorder(as.numeric(model_id))
        )
    
    
    # 1A. ROC Plot
    
    p1 <- model_metrics_tbl %>%
        ggplot(aes_string("fpr", "tpr", color = "model_id", linetype = order_by)) +
        geom_line(size = size) +
        theme_tq() +
        scale_color_tq() +
        labs(title = "ROC", x = "FPR", y = "TPR") +
        theme(legend.direction = "vertical")
    
    # 1B. Precision vs Recall
    
    p2 <- model_metrics_tbl %>%
        ggplot(aes_string("recall", "precision", color = "model_id", linetype = order_by)) +
        geom_line(size = size) +
        theme_tq() +
        scale_color_tq() +
        labs(title = "Precision Vs Recall", x = "Recall", y = "Precision") +
        theme(legend.position = "none")
    
    
    # 2. Gain / Lift
    
    get_gain_lift <- function(model_id, test_tbl) {
        
        model_h2o <- h2o.getModel(model_id)
        perf_h2o  <- h2o.performance(model_h2o, newdata = as.h2o(test_tbl)) 
        
        perf_h2o %>%
            h2o.gainsLift() %>%
            as.tibble() %>%
            select(group, cumulative_data_fraction, cumulative_capture_rate, cumulative_lift)
        
    }
    
    gain_lift_tbl <- leaderboard_tbl %>%
        mutate(metrics = map(model_id, get_gain_lift, newdata_tbl)) %>%
        unnest() %>%
        mutate(
            model_id = as_factor(model_id) %>% 
                fct_reorder(!! order_by_expr, .desc = ifelse(order_by == "auc", TRUE, FALSE)),
            auc  = auc %>% 
                round(3) %>% 
                as.character() %>% 
                as_factor() %>% 
                fct_reorder(as.numeric(model_id)),
            logloss = logloss %>% 
                round(4) %>% 
                as.character() %>% 
                as_factor() %>% 
                fct_reorder(as.numeric(model_id))
        ) %>%
        rename(
            gain = cumulative_capture_rate,
            lift = cumulative_lift
        ) 
    
    # 2A. Gain Plot
    
    p3 <- gain_lift_tbl %>%
        ggplot(aes_string("cumulative_data_fraction", "gain", 
                          color = "model_id", linetype = order_by)) +
        geom_line(size = size) +
        geom_segment(x = 0, y = 0, xend = 1, yend = 1, 
                     color = "black", size = size) +
        theme_tq() +
        scale_color_tq() +
        expand_limits(x = c(0, 1), y = c(0, 1)) +
        labs(title = "Gain",
             x = "Cumulative Data Fraction", y = "Gain") +
        theme(legend.position = "none")
    
    # 2B. Lift Plot
    
    p4 <- gain_lift_tbl %>%
        ggplot(aes_string("cumulative_data_fraction", "lift", 
                          color = "model_id", linetype = order_by)) +
        geom_line(size = size) +
        geom_segment(x = 0, y = 1, xend = 1, yend = 1, 
                     color = "black", size = size) +
        theme_tq() +
        scale_color_tq() +
        expand_limits(x = c(0, 1), y = c(0, 1)) +
        labs(title = "Lift",
             x = "Cumulative Data Fraction", y = "Lift") +
        theme(legend.position = "none")
    
    
    # Combine using cowplot
    p_legend <- get_legend(p1)
    p1 <- p1 + theme(legend.position = "none")
    
    p <- cowplot::plot_grid(p1, p2, p3, p4, ncol = 2) 
    
    p_title <- ggdraw() + 
        draw_label("H2O Model Metrics", size = 18, fontface = "bold", 
                   colour = palette_light()[[1]])
    
    p_subtitle <- ggdraw() + 
        draw_label(glue("Ordered by {toupper(order_by)}"), size = 10,  
                   colour = palette_light()[[1]])
    
    ret <- plot_grid(p_title, p_subtitle, p, p_legend, 
                     ncol = 1, rel_heights = c(0.05, 0.05, 1, 0.05 * max_models))
    
    h2o.show_progress()
    
    return(ret)
    
}

