

data_nb = data_nb %>% filter(!is.na(prop_home_device_zip))

fitted_values = fs3$fitted.values %>% as.data.frame()
names(fitted_values) = "fitted_values"
data_nb = cbind(data_nb, fitted_values)

local_iv_means = data_nb %>% group_by(postal_code) %>% summarise(prop_local = mean(proption_BigBrands_naics_postal_open),prop_fitted = mean(fitted_values))

#plot1 = data_nb %>% filter(!is.na(meanI)) %>% ggplot()  + stat_summary_bin(aes(y = meanI, x = proption_BigBrands_naics_postal_open),fun='mean',bins = 20,geom = "point",color="blue")  + xlab("Prop. Brach Est. Open") + ylab("Mean Income") + labs(title ="Mean Income vs. Prop. Branch Est. Open") + ylim(0,max(data_nb$meanI,na.rm=T))
#ggsave("plots/iv/bin_IV_income.png",plot1)

### Fitted Values ###
plot2 = data_nb %>% filter(!is.na(meanI)) %>% ggplot() + stat_summary_bin(aes(y = meanI, x = fitted_values),fun='mean',bins = 20,geom = "point",color="red")  + xlab("Prop. Brach Est. Open")+ ylab("Mean Income")  + labs(title ="Mean Income vs. Prop. Branch Est. Open (fit)") + ylim(0,max(data_nb$meanI,na.rm=T)) 
ggsave("plots/iv/bin_IV_income_fitted.png",plot2) 


plot3 = data_nb %>% filter(!is.na(MeanLogDev)) %>% ggplot() + stat_summary_bin(aes(y = MeanLogDev, x = fitted_values),fun='mean',bins = 20,geom = "point",color="red")  + xlab("Prop. Brach Est. Open")+ ylab("Mean Log Deviation of Income")  + labs(title ="Income Inequality vs. Prop. Branch Est. Open (fit)") + ylim(0,max(data_nb$MeanLogDev,na.rm=T)) 
ggsave("plots/iv/bin_IV_incomeIneq_fitted.png",plot3) 


plot4 = data_nb %>% filter(!is.na(PercentWhite)) %>% ggplot()  + stat_summary_bin(aes(y = PercentWhite, x = fitted_values),fun='mean',bins = 20,geom = "point",color="red")  + ylab("PercentWhite") + xlab("Prop. Brach Est. Open") + labs(title ="PercentWhite vs. Prop. Branch Est. Open (fit)") + ylim(0,1)
ggsave("plots/iv/bin_IV_PercentWhite_fitted.png",plot4)

plot5 = data_nb %>% filter(!is.na(PercentAsian)) %>% ggplot()  + stat_summary_bin(aes(y = PercentAsian, x = fitted_values),fun='mean',bins = 20,geom = "point",color="red")  + ylab("PercentAsian") + xlab("Prop. Brach Est. Open") + labs(title ="PercentAsian vs. Prop. Branch Est. Open (fit)") + ylim(0,1)
ggsave("plots/iv/bin_IV_PercentAsian_fitted.png",plot5)

###########################
### Instrument Variable ###
###########################

plot2 = data_nb %>% filter(!is.na(meanI)) %>% ggplot() + stat_summary_bin(aes(y = meanI, x = BrandPostalProp),fun='mean',bins = 20,geom = "point",color="red")  + xlab("National Chain Opening Exposure")+ ylab("Mean Income")  + labs(title ="Mean Income vs. National Chain Opening Exposure") + ylim(0,max(data_nb$meanI,na.rm=T)) + theme_bw()
ggsave("plots/iv/bin_IV_income_IV.png",plot2) 


plot3 = data_nb %>% filter(!is.na(MeanLogDev)) %>% ggplot() + stat_summary_bin(aes(y = MeanLogDev, x = BrandPostalProp),fun='mean',bins = 20,geom = "point",color="red")  + xlab("National Chain Opening Exposure")+ ylab("Mean Log Deviation of Income")  + labs(title ="Income Inequality vs. National Chain Opening Exposure") + ylim(0,max(data_nb$MeanLogDev,na.rm=T)) + theme_bw()
ggsave("plots/iv/bin_IV_incomeIneq_IV.png",plot3) 


plot4 = data_nb %>% filter(!is.na(PercentWhite)) %>% ggplot()  + stat_summary_bin(aes(y = PercentWhite, x = BrandPostalProp),fun='mean',bins = 20,geom = "point",color="red")  + ylab("Prop. White") + xlab("National Chain Opening Exposure") + labs(title ="Prop. White vs. National Chain Opening Exposure") + ylim(0,1) + theme_bw()
ggsave("plots/iv/bin_IV_PercentWhite_IV.png",plot4)

plot5 = data_nb %>% filter(!is.na(PercentAsian)) %>% ggplot()  + stat_summary_bin(aes(y = PercentAsian, x = BrandPostalProp),fun='mean',bins = 20,geom = "point",color="red")  + ylab("Prop. Asian") + xlab("National Chain Opening Exposure") + labs(title ="Prop. Asian vs. National Chain Opening Exposure") + ylim(0,1) + theme_bw()
ggsave("plots/iv/bin_IV_PercentAsian_IV.png",plot5)

plots = grid.arrange(plot2,plot3,plot4,plot5,ncol=2)
ggsave("plots/iv/IV_plots.png",plots,width=12,height=12)
