### SETUP
prj_path <- wow::init("RWA_potato", "agworkflows")

predRFs <- readRDS(file.path(prj_path, "/data/intermediate/predRFs.RDS"))

predRFsl <- predRFs |>
  tidyr::gather(variable, value, dYp0:dYp1) |>
  dplyr::mutate(variable = plyr::mapvalues(variable, from = c("dYp0", "dYp1"), to = c("With refY", "without refY")))


library(ggplot2)

#Visualize yield effects
png(file.path(prj_path, "img/9_yield_effects.png"))

	ggplot(predRFsl, aes(x = value, y = dY)) + 
	  geom_point(alpha=.33, shape=16) +
	  facet_wrap(~variable) +
	  geom_text(data = predRFsl |> 
				  dplyr::group_by(variable) |> 
				  dplyr::summarise(rmse = sqrt(sum((value - dY)**2)/dplyr::n()),
							value = -1,	dY = 20),
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

dev.off()


qd <- readRDS(file.path(prj_path, "/data/intermediate/qd.RDS"))
ds <- readRDS(file.path(prj_path, "/data/intermediate/potato_fertiliser_trial_data_blup.RDS"))

png(file.path(prj_path, "img/9_Nvar.png"))

	res <- qd[qd$P == 22 & qd$K ==42, ]
	res |>
	  dplyr::filter(grepl("RwaSIS", TLID) | grepl("RS", TLID)) |>
	  ggplot()+
	  geom_point(aes(x = N, y=yp))+
	  geom_point(aes(x = N, y=ypRF), colour="blue")+
	  geom_point(data=ds[ds$TLID %in% res$TLID & ds$P == 22 & ds$K == 42 & (grepl("RwaSIS", ds$TLID) | grepl("RS", ds$TLID)),], 
				 aes(x=N, y=blup), colour="red", size = 4)+
	  facet_wrap(~TLID) + 
	  xlab("\nN fertilizer application rate (kg N/ha)") +
	  ylab("Potato tuber yield [t/ha]\n")+
	  theme_gray() +
	  theme(axis.title = element_text(size = 14, face="bold"),
			axis.text = element_text(size = 14),
			strip.text = element_text(size = 14, face="bold"))

dev.off()



png(file.path(prj_path, "img/9_Pvar.png"))

	res <- qd[qd$N == 51 & qd$K ==42, ]
	res |>
	  dplyr::filter(grepl("RwaSIS", TLID) | grepl("RS", TLID)) |>
	  ggplot()+
	  geom_point(aes(x = P, y=yp))+
	  geom_point(aes(x = P, y=ypRF), colour="blue")+
	  geom_point(data=ds[ds$TLID %in% res$TLID & round(ds$N) == 51 & ds$K == 42 & (grepl("RwaSIS", ds$TLID) | grepl("RS", ds$TLID)),], 
				 aes(x=P, y=blup), colour="red", size = 4)+
	  facet_wrap(~TLID) + 
	  xlab("\nP fertilizer application rate (kg P/ha)") +
	  ylab("Potato tuber yield [t/ha]\n")+
	  theme_gray() +
	  theme(axis.title = element_text(size = 14, face="bold"),
			axis.text = element_text(size = 14),
			strip.text = element_text(size = 14, face="bold"))

dev.off()



png(file.path(prj_path, "img/9_Kvar.png"))

	res <- qd[qd$N ==51 & qd$P == 22, ]
	res |>
	  dplyr::filter(grepl("RwaSIS", TLID) | grepl("RS", TLID)) |>
	  ggplot()+
	  geom_point(aes(x = K, y=yp))+
	  geom_point(aes(x = K, y=ypRF), colour="blue")+
	  geom_point(data=ds[ds$TLID %in% res$TLID & round(ds$N) == 51 & round(ds$P) == 22 & (grepl("RwaSIS", ds$TLID) | grepl("RS", ds$TLID)),], 
				 aes(x=K, y=blup), colour="red", size = 4)+
	  facet_wrap(~TLID) + 
	  xlab("\nK fertilizer application rate (kg P/ha)") +
	  ylab("Potato tuber yield [t/ha]\n")+
	  theme_gray() +
	  theme(axis.title = element_text(size = 14, face="bold"),
			axis.text = element_text(size = 14),
			strip.text = element_text(size = 14, face="bold"))

dev.off()
