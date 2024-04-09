# functions for develop IOW to balance the sample between reference and index
# Covariate balance and weight development function was adapted from 
# Eleanor Hayes-Larson's KHANDLE weighting code (https://github.com/Mayeda-Research-Group/KHANDLE-weighting) and 
# Yingyan Wu's sample selection code (not published on Github yet when this was published)

checkcovbal <- function(data, weightvar, treatvar){
  # data = charls_covbal_1518
  # weightvar = "unweight"
  # treatvar = "treat_18"
  temp <- twang::bal.stat(data = data, 
                          vars = selected_vars,
                          w.all = data[, weightvar],
                          treat.var = treatvar, # 1 = 2018 or 2020
                          sampw = data$unweight, 
                          estimand = "ATT", 
                          na.action = "exclude",
                          multinom = F)
  
  temp[[1]][, "var"] <- rownames(temp[[1]])
  covbal_toplot <- do.call("rbind", temp)
  
  # Label the variables
  covbal_toplot %<>%
    filter(var %in% order_var) %>%
    arrange(desc(std.eff.sz)) %>%
    mutate(label = case_when(
      var == "age_y" ~ "Age (Years)",
      var == "female" ~ "Female",
      var == "childhood_edu:2" ~ "Vocational school/middle school/high school",
      var == "childhood_edu:3" ~ "College & above",
      var == "childhood_edu:1" ~ "Elementary school or less",
      var == "rural" ~ "Rural residence",
      var == "mstat" ~ "Married",
      var == "work_status:1" ~ "Agriculture employed",
      var == "work_status:2" ~ "Non-agriculture employed",
      var == "work_status:3" ~ "Unemployed",
      var == "work_status:4" ~ "Retired",
      var == "work_status:5" ~ "Never work"
    ))
  covbal_toplot %<>%
    mutate(var = factor(var, levels = rev(order_var))) %>%
    arrange(desc(var))
  covbal_toplot$label <- factor(covbal_toplot$label, 
                                levels = rev(covbal_toplot$label))
  
  # Generate data to plot covariate balance
  covbal_plot <- covbal_toplot %>%
    ggplot(aes(x = std.eff.sz, y = label)) +
    geom_point(size = 2, color = "#0072B2") +
    geom_vline(xintercept = 0) +
    xlab("Standardized mean diff Mean(Ref) - Mean(Index)/SD(Ref)") +
    theme_bw() +
    # geom_vline(xintercept = c(-0.2, 0.2), linetype = "dashed",
    #            color = "dark grey") +
    # sclae_x_continuous
    theme(aspect.ratio = 5/3, panel.grid.minor.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_text(size = 10, face = "bold"),
          legend.position = "none")
  
  return(list("covbal_data" = covbal_toplot, "covbal_plot" = covbal_plot))
}

covbal_comp_plot <- function(weighted_covbal_data, unweighted_covbal_data,
                             ref, index){
  covbal_toplot <- weighted_covbal_data %>% mutate(IOW = "1") %>%
    rbind(unweighted_covbal_data %>% mutate(IOW = "0"))
  
  plot <- covbal_toplot %>%
    ggplot(aes(x = std.eff.sz, y = label)) +
    geom_point(aes(shape = IOW), size = 2, color = "#0072B2") +
    scale_shape_manual(values = c(21, 16)) +
    geom_vline(xintercept = 0) +
    xlab(paste0("Standardized mean diff\nmean(Wave ",ref, ") - mean(Wave ", index,
                ")/SD(Wave ", ref, ")")) +
    theme_bw() +
    # geom_vline(xintercept = c(-0.2, 0.2), linetype = "dashed",
    #            color = "dark grey") +
    theme(
      aspect.ratio = 5/3,
      panel.grid.minor.x = element_blank(),
      axis.title.y = element_blank(),
      text = element_text(size = 10, color = "black"),
      axis.text.x = element_text(size = 10, color = "black"),
      axis.text.y = element_text(size = 10, color = "black", face = "bold"),
      legend.position = "none")
  
  return(plot)
}

weight_dev <- function(dataset, pS_Z_formula, index, treatvar, pS_wave){
  # dataset = charls_covbal_1518
  # pS_Z_formula = "treat_18 ~ childhood_edu + age_y" 
  # index = "1"
  # treatvar = "treat_18"
  # pS_wave = "pS_18"
  pS_Z_model <- glm(as.formula(pS_Z_formula),
                    family = binomial(link = logit),
                    weights = unweight,
                    data = dataset)
  
  dataset %<>%
    mutate(!!paste0("pS_Z", index) := predict(pS_Z_model, type = "response"),
           # W = (1-PS_Z)/PS_Z
           !!paste0("W", index) := 
             (1 - !!sym(paste0("pS_Z", index)))/!!sym(paste0("pS_Z", index)),
           # SW = W*PS/(1-PS)
           !!paste0("SW", index) := case_when(
             !!sym(treatvar) == 1 ~ 
               !!sym(paste0("W", index))*get(pS_wave)/(1-get(pS_wave)),
             !!sym(treatvar) == 0 ~ 1),
           # SW_twang = SW*samplingwt
           !!paste0("SW", index, "_twang") := 
             !!sym(paste0("SW", index))*unweight)
  
  # Truncate SW*samplingwt at 99% for those in treatment group
  sw_99th <- quantile(dataset %>% filter(!!sym(treatvar) == 1) %>% 
                        pull(!!sym(paste0("SW", index, "_twang"))), 0.99)
  dataset %<>%
    mutate(!!paste0("SW", index, "_twang_trunc99") := 
             case_when(!!sym(treatvar) == 1 & 
                         !!sym(paste0("SW", index, "_twang")) > sw_99th ~ sw_99th,
                       TRUE ~ !!sym(paste0("SW", index, "_twang"))))
  
  # Truncation pending
  return(dataset %>%
           select(paste0("pS_Z", index), paste0("W", index),
                  paste0("SW", index), paste0("SW", index, "_twang"),
                  paste0("SW", index, "_twang_trunc99")))
}