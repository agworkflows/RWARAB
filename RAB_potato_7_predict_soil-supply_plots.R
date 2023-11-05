### SETUP
prj_path <- wow::init("RWA_potato", "agworkflows")


## Predict indigenous nutrient supply from soil data #

ds <- readRDS(file.path(prj_path, "/data/intermediate/potato_fertiliser_trial_data_blup.RDS"))
INS <- readRDS(file.path(prj_path, "data/intermediate/INS.RDS"))

pINS <- INS |>
  dplyr::select(TLID, N_base_supply, P_base_supply, K_base_supply, N_pred, P_pred, K_pred) |>
  tidyr::gather(variable, value, -TLID) |>
  dplyr::mutate(nutrient = substr(variable, 1, 1),
         nutrient = factor(nutrient, levels = c("N", "P", "K")),
         variable = ifelse(grepl("pred", variable), "RandomForest", "revQUEFTS")) |>
  tidyr::spread(variable, value) |>
  dplyr::left_join(ds |> dplyr::select(TLID, expCode, season) |> unique(), by="TLID") |>
  dplyr::mutate(season = ifelse(grepl("A", season), "A", "B")) |>
  #mutate(RandomForest = ifelse(nutrient == "N" & RandomForest >= 120, NA, 
  #                             ifelse(nutrient == "P"& RandomForest >= 30, NA,
  #                                    ifelse(nutrient == "K" & RandomForest >= 150, NA, RandomForest)))) |>
  na.omit()
  

library(ggplot2)

#Create plot to demonstrate ranges in supply by expCode and season combinations:
png(file.path(prj_path, "img/7_rev_randomforest.png"))

#plot demonstrating reverse QUEFTS calculated N, P and K supply versus RF predictions using LOOCV:
ggplot(pINS, aes(x = RandomForest, y = revQUEFTS)) + 
  geom_abline(intercept = 0, slope = 1) +
  geom_point(aes(fill = season, shape=expCode), size = 3) + 
  scale_shape_manual(values = 21:23) +
  scale_fill_manual(values = c("grey90", "grey50"))+
  facet_wrap(~nutrient, nrow=1, scales="free") +
  ggpmisc::stat_poly_line(formula = y ~ x, se = F) +
  ggpmisc::stat_poly_eq(ggpmisc::use_label(c("eq", "R2")),
                        formula = y ~ x, size = 6,
                        label.y = .975) +
  geom_text(data = pINS |> 
              dplyr::group_by(nutrient) |> 
              dplyr::summarise(rmse = sqrt(sum((revQUEFTS - RandomForest)**2)/ dplyr::n()),
                        RandomForest = 0) |>
              dplyr::mutate(revQUEFTS = c(200, 200, 200)*0.95),
            aes(label = paste0("rmse = ", round(rmse*100)/100)),
            size = 6, hjust = 0) +
  xlim(0, NA)+
  ylim(0, 200)+
  xlab("\nRandom Forest predicted soil nutrient supply [kg/ha]")+
  ylab("Reverse QUEFTS calculated supply [kg/ha]\n") +
  theme_gray() +
  theme(strip.text = element_text(size = 14, face="bold"),
        axis.text = element_text(size = 14),
        legend.position = "none",
        axis.title = element_text(size = 15, face="bold"))

dev.off()

