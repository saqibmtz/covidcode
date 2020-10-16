library(ggplot2)
library(datasets)
library(lfe)
library(dplyr)
library(broom)

themePaul <- function () {
    theme_bw(base_size=12) %+replace%
        theme(
            legend.position = "bottom",
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border= element_blank(),
            axis.line = element_line(colour = "black"),
            axis.text=element_text(size=10),
            axis.title=element_text(size=10))
}

binscatter <- function(data, y, x, bins=20, discrete=FALSE, scatter=FALSE,
                       theme=themePaul, fitline=TRUE, controls=c(), absorb=c("0"),
                       clustervars=c("0"), pos="bottom right") {
    if(length(controls) == 0) {
        formula = as.formula(paste(y, "~", x  , "|" ,
            paste(absorb,sep="",collapse=" + ") , "|" , "0" ,  "|" ,
            paste(clustervars,sep="",collapse=" + "), sep=" "))
    }
    if(length(controls)!=0) {
        formula = as.formula(paste(y, "~", x, "+", paste(controls,sep="",collapse=" + ") , "|" ,
                        paste(absorb,sep="",collapse=" + ") , "|" , "0" ,  "|" ,
                            paste(clustervars,sep="",collapse=" + "), sep=" "))

        y_res_formula = as.formula(paste(y, "~", paste(controls,sep="",collapse=" + ") , "|" ,
                            paste(absorb,sep="",collapse=" + ") , "|" , "0" ,  "|" ,
                            paste(c("0"),sep="",collapse=" + "), sep=" "))
        x_res_formula = as.formula(paste(x, "~", paste(controls,sep="",collapse=" + ") , "|" ,
                            paste(absorb,sep="",collapse=" + ") , "|" , "0" ,  "|" ,
                            paste(c("0"),sep="",collapse=" + "), sep=" "))
        controls <- data[,controls]
    }
    x_label = x
    y_label = y
    x <- data[,x]
    y <- data[,y]
    f <- felm(formula, data=data)
    print(tidy(f)[2,2:3])

    print("DID REGRESSION!! ")
    print(tidy(f))
    beta <- paste("beta", round(tidy(f)[2,2], 3), sep="=")
    se <-   paste("s.e.", round(tidy(f)[2,3], 3), sep="=")

    print("DID BETAS!! ")
    if(length(controls) != 0) {
        f_Xres <- felm(x_res_formula, data=data)
        f_Yres <- felm(y_res_formula, data=data)
        x <- f_Xres$residuals + mean(x)
        y <- f_Yres$residuals + mean(y)
    }
    g <- ggplot(data, aes(x = x , y= y))  + themePaul() +
        xlab(x_label) + ylab(y_label)

    print("DID BASEPLOT!! ")
    if (scatter == TRUE) {
        g <- g + geom_point()
    }
    if (discrete == TRUE) {
        g <- g + stat_summary(fun.y = "mean",  colour = "#0072B2", size = 2, geom="point")
    }
    else {
        g <- g + stat_summary_bin(fun.y = "mean",  colour = "#0072B2", size = 2, geom="point", bins=20)
    }
    if (fitline == TRUE) {
        g <- g + geom_smooth(method='lm',formula=y~x, se=FALSE, color="#D55E00", size=1)
        posx <- c(Inf, Inf, -Inf, -Inf)
        posy <- c(Inf, -Inf, Inf, -Inf)
        posname <- c("top right", "bottom right", "top left", "bottom left")
        adjh <- c(1,1,-1,-1)
        adjv <- c(1,-1,1,-1)
        posdf <- data.frame(posx, posy, adjh, adjv, row.names=posname)

        print(posdf)
        g <- g +
            geom_text(aes(x=Inf, y=-Inf,hjust=1, vjust=-2.5, label=beta)) +
            geom_text(aes(x=Inf, y=-Inf,hjust=1, vjust=-1, label=se))
    }
    return(g)
}



########################## Second COde #######################################

data_nb['DateZip'] = paste(data_nb$date, data_nb$postal_code)
  #reg_all <- felm(formula(formula),data=data)
  reg_all <- felm(cases ~ BrandPostalProp, data_nb)

  #reg_y <- felm(formula(paste(y, "~", controls, sep="")), data=data)
  reg_y <- felm(deaths ~ 1 | newfactor + postal_code + date, data_nb)

  #reg_x <- felm(formula(paste(key_var, "~", controls, sep="")), data=data)
  reg_x <- felm(BrandPostalProp ~ 1 | newfactor + postal_code + date, data_nb)

resid_y <- resid(reg_y) + mean(data_nb$deaths)
resid_x <- resid(reg_x) + mean(data_nb$BrandPostalProp)

plot2 =  ggplot() + stat_summary_bin(aes(y = resid_y, x = resid_x),fun='mean',bins = 30,geom = "point",color="red")  + xlab("BrandPostalProp (IV) ")+ ylab("Number of COVID Deaths")  + labs(title ="Covid Deaths vs. BrandPostalProp (IV)") 
ggsave("plots/eventstudy/bin_IV_covidDeaths_fitted_FE_DateZipDatexNaics_30bins.jpg",plot2) 


data_nb$newvar = as.integer(data_nb$BrandPostalProp*30)

temp = data_nb %>% group_by(newvar) %>% summarise(count = sum(!is.na(deaths)))
temp$newvar = temp$newvar/30
p1 = ggplot(temp, aes(x = newvar, y=count)) + geom_line() + xlab("Instrument Variable") + ylab("Number of Observations for deaths")
ggsave("plots/IV_Death.jpg",p1)5