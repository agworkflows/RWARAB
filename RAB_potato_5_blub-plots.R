############## START SETUP
prj_path <- agvise::setup_project("RWA_potato", "agworkflows")
################ SETUP END


ds <- readRDS(file.path(prj_path, "/data/intermediate/potato_fertiliser_trial_data_blub.RDS"))

#plot showing relationship between observations (with random error) and BLUPs (without random error)
base_plot <- function() {
	plot(ds$TY, ds$blup, ylab="BLUP tuber yield (t/ha)", xlab="Observed tuber yield (t/ha)", las=1)
	m <- lm(blup ~ TY, data=ds)
	cf <- round(coefficients(m), 1)
	abline(m, lwd=2, col="red")
	text(0, 55, labels=paste0("y = ", cf[1], " + ", cf[2], "x"), pos=4) 
}
#base_plot()

library(ggplot2)

png(file.path(prj_path, "img/blub_vs_raw.png"))

	ggplot(ds, aes(x = blup, y = TY)) + 
	  geom_point(alpha=.33, shape=16) +
	  geom_abline(intercept = 0, slope = 1) +
	  ggpmisc::stat_poly_line(formula = y ~ x, se = F) +
	  ggpmisc::stat_poly_eq(ggpmisc::use_label(c("eq", "R2")),
				   formula = y ~ x, size = 6)+
	  xlab("\nBLUP tuber yield [t/ha]") +
	  ylab("Observed tuber yield [t/ha]\n") +
	  theme_gray()+
	  theme(axis.title = element_text(size = 14, face="bold"),
			axis.text = element_text(size = 14))
}
dev.off()

#plot illustrating that the elimination of random error results in more meaningful structure in yield response:
png(file.path(prj_path, "img/blub_structure1.png"))

ds |>
  tidyr::gather(variable, value, c(TY, blup)) |>
  dplyr::group_by(TLID, variable) |>
  dplyr::mutate(refY = ifelse(N > 75 & P > 30 & K > 50, value, NA),
         refY = mean(refY, na.rm=TRUE),
         dY = refY - value,
         variable = factor(variable, levels=c("TY", "blup")),
         variable = plyr::mapvalues(variable,
                              from = c("TY", "blup"),
                              to = c("Raw observations", "BLUPs"))) |>
  dplyr::filter(!(N > 75 & P > 30 & K > 50)) |>
  ggplot(aes(x = refY, y = dY)) + 
  #geom_point(alpha=.33, shape=16) + 
  geom_point(aes(shape = variable)) +
  scale_shape_manual(values = c(3, 16)) +
  facet_wrap(~variable) + 
  #facet_grid(expCode ~ variable) +
  geom_hline(yintercept = 0) +
  xlab("\nYield in reference treatment [t/ha]") +
  ylab("Yield difference relative to reference treatment [t/ha]\n") +
  theme_gray()+
  theme(axis.title = element_text(size = 14, face="bold"),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face="bold"),
        legend.position = "none")

dev.off()


png(file.path(prj_path, "img/blub_structure2.png"))

ds |>
  dplyr::filter(TLID %in% sample(unique(ds$TLID), 12, replace = F)) |>
  ggplot(aes(x = treat, y = blup)) +
  geom_point(size = 3) + 
  geom_point(aes(y = TY), shape = 3, size = 3) +
  facet_wrap(~TLID, scales = "free_x") + 
  ylab("Potato tuber yield [t/ha]\n") +
  theme_gray()+
  theme(axis.title.y = element_text(size = 14, face="bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 14),
        strip.text = element_text(size = 14, face="bold"))

dev.off()

