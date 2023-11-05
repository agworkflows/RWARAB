### SETUP
prj_path <- wow::init("RWA_potato", "agworkflows")


ds <- readRDS(file.path(prj_path, "/data/intermediate/potato_fertiliser_trial_data_blup.RDS"))
py <- readRDS(file.path(prj_path, "data/intermediate/py.RDS"))

library(ggplot2)

png(file.path(prj_path, "img/8_rev_randomforest.png"))

py |> dplyr::left_join(ds |> dplyr::select(TLID, expCode, season) |> unique(), by="TLID") |>
  #filter(Nlim == "limiting", Plim == "non-limiting" & Klim == "non-limiting") |>
  dplyr::mutate(refY = ifelse(N > 75 & P > 30 & K > 50, "Reference treatment", "Other treatments")) |>
  ggplot(aes(x = Yp, y = Yb)) + 
  geom_point(alpha = .33, shape = 16) + 
  geom_abline(slope= 1, intercept = 0) + 
  #stat_poly_line(se = F) +
  #stat_poly_eq(aes(label = after_stat(eq.label)), size=6) +
  #stat_poly_eq(label.y = 0.9, size = 6) +
  #facet_wrap(~refY) +
  ggpmisc::stat_poly_line(formula = y ~ x, se = F) +
  ggpmisc::stat_poly_eq(ggpmisc::use_label(c("eq", "R2")),
                        formula = y ~ x, size = 6) +

  #facet_wrap(~expCode+season, ncol=3) + 
  xlab("\nPredicted tuber yield (t/ha)")+
  ylab("BLUP potato tuber yield (t/ha)\n")+
  xlim(0, 62.5)+
  ylim(0, 62.5)+
  theme_gray()+
  theme(axis.title = element_text(size = 14, face="bold"),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face="bold"))
  

pyr <- py |>
  tidyr::gather(variable, value, Yq:Yo) |>
  dplyr::group_by(TLID, N, P, K, variable) |>
  dplyr::summarise(value = mean(value)) |>
  dplyr::mutate(treat = ifelse(N>75 & P>30 & K>50, "ref", "other")) |>
  dplyr::group_by(TLID, variable) |>
  dplyr::mutate(refY = mean(ifelse(treat == "ref", value, NA), na.rm=TRUE),
         dY = refY - value) |>
  dplyr::filter(treat != "ref") |>
  dplyr::select(-treat, -value, -refY) |>
  tidyr::spread(variable, dY) |>
  tidyr::gather(variable, value, c(Yq, Yp, Yo, Yn)) |>
  dplyr::mutate(variable = plyr::mapvalues(variable, 
                              from = c("Yq", "Yp", "Yn"),
                              to = c("supply from reverse QUEFTS", "supply by RF prediction", "simple medians for supply"))) |>
  dplyr::ungroup()


pyr |>
  dplyr::filter(variable != "Yo") |>
  #filter(Nlim == "limiting", Plim == "non-limiting" & Klim == "non-limiting") |>
  dplyr::mutate(variable = as.character(variable)) |>
  ggplot(aes(x = value, y = Yb)) + 
  geom_point(alpha=.33, shape=16) +
  facet_wrap(~variable) +
  geom_text(data = pyr |> 
              dplyr::filter(variable != "Yo") |> 
              dplyr::group_by(variable) |> 
              dplyr::summarise(rmse = sqrt(sum((value - Yb)**2)/ dplyr::n()),
                        value = -1,
                        Yb = 20),
            aes(label = paste0("rmse = ", round(rmse*100)/100)),
            size = 6, hjust = 0) +
  xlab("\nPredicted potato tuber yield difference to reference treatment [t/ha]") +
  ylab("BLUP potato tuber yield difference to reference treatment [t/ha]\n") +
  geom_abline(intercept = 0, slope = 1) +
  ggpmisc::stat_poly_line(formula = y ~ x, se = F) +
  ggpmisc::stat_poly_eq(ggpmisc::use_label(c("eq")),
                        formula = y ~ x, size = 6) +
  theme_gray()+
  theme(axis.title = element_text(size = 14, face="bold"),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face="bold"))

