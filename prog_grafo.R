#===============================================================================
#          GENERATION OF RELATIVE INCOME DYNAMICS GRAPH
# Course: Econometric for the elaboration of Thesis and Research Documents
# Institution: LAMBDA Economics and Finance Training Center
# Teacher: LEONEL DURAND BAÑOS
#===============================================================================


# Section 0 ---------------------------------------------------------------
  ## Remove
    getwd()
    rm(list=ls())
  ## install.packages()
    install.packages("astsa")
    install.packages("readxl")
    install.packages("ggplot2")
    install.packages("plotly")
    install.packages("grid")
  ## library()
    library(astsa)
    library(readxl)
    library(ggplot2)
    library(plotly)
    library(grid)
# Section 1 ---------------------------------------------------------------
  ## Descargamos datos
    Dinamica<-read_excel("Datos.xlsx",
                     sheet="Dinamica", 
                     range = "A3:F102")
  ## ggplot
    p1<- ggplot(Dinamica,
                aes(x=ing_60,
                    y=ing_17,
                    col=Grupo)) +
    geom_point(size=3, 
               stroke=1) +
    xlab("Logaritmo del % del PBI per capita PPP respecto 
         al de Estados Unidos (1960)") +
    ylab("Logaritmo del % del PBI per capita PPP respecto 
         al de Estados Unidos (2017)") +
    geom_vline(xintercept=c(log(0.1*100),
                            log(0.5*100)),
               col="gray50",
               linetype="dashed",
               size=0.5) +
    geom_hline(yintercept=c(log(0.1*100),
                            log(0.5*100)),
               col="gray50",
               linetype="dashed",
               size=0.5) +
    theme_bw()+
    theme(legend.position = 'none', 
          plot.caption = element_text(hjust = 0),
          plot.title = element_text(hjust = 0.5)) +
    annotate(geom="text", 
             x=c(1.5,3,4.5), 
             y=c(.3,.3,.3), 
             label=c("Ingreso Bajo",
                     "Ingreso Medio",
                     "Ingreso Alto"),
             color="brown",
             fontface="bold") +
    annotate(geom="text", 
             x=c(.9,.9,.9), 
             y=c(1.15,3.2,4.7), 
             label=c("Ingreso Bajo",
                     "Ingreso Medio",
                     "Ingreso Alto"),
             color="brown",
             angle=90, 
             fontface="bold")+
    scale_colour_manual(name="", 
                        values = c("red",
                                   "royalblue", 
                                   "springgreen3"))
    etiquetas<- c("Peru",
                  "Chile",
                  "Colombia",
                  "Mexico",
                  "Corea del Sur",
                  "Taiwan",
                  "Hong Kong",
                  "Singapur",
                  "Japon")
# Section 2 ---------------------------------------------------------------
  ## install.packages('ggrepel')
    library(ggrepel)
    windows()
    (p11<- p1 + 
        geom_text_repel(aes(label = Pais), 
                            color = "gray20", 
                            lwd = 4.5, 
                            data = filter(Dinamica, 
                                          Pais %in% etiquetas), 
                            force = 15))


     