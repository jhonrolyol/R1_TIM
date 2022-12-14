#===============================================================================
#          EVALUATION OF THE PRESENCE OF UNIT ROOT
# Course: Econometric for the elaboration of Thesis and Research Documents
# Institution: LAMBDA Economics and Finance Training Center
# Teacher: LEONEL DURAND BAÑOS
#===============================================================================

#====================
# TEST DICKEY FULLER -----------------------------------------------------------
#====================

# Section 0 ---------------------------------------------------------------
  ## Remove
    getwd()
    rm(list=ls())
  ## install.packages()
    install.packages("astsa")
    install.packages("readxl")
    install.packages("urca")
  ## library()
    library(astsa)
    library(readxl)
    library(urca)
  ## Note
    ### paquetes<- c('astsa','readxl','urca')
    ### install.packages(paquetes)
    
# Section 1 ---------------------------------------------------------------
  data<- read_excel("Datos.xlsx",
                  sheet = "uroot", 
                  range = "V4:AH72")
# Section 2 ---------------------------------------------------------------
  ## Establecemos los limites que definen a los paises de ingresos medios. 
  ## Se tienen los limites inferior y superior al 10% y 50% del logaritmo 
  ## del ingreso relativo de USA, respectivamente.
    ti<- mean(ts(data[,'linf']))
    ts<- mean(ts(data[,'lsup']))
    
#================================
# GENERAMOS LAS SERIES DE TIEMPO -----------------------------------------------
#================================
    
# Section 3 ---------------------------------------------------------------
  per.ts<- ts(data$per,start = c(1950,1),frequency = 1)
  chil.ts<- ts(data$chil,start = c(1950,1),frequency = 1)
  col.ts<- ts(data$col,start = c(1950,1),frequency = 1)
  mx.ts<- ts(data$mx,start = c(1950,1),frequency = 1)
  cor.ts<- ts(data$cor,start = c(1950,1),frequency = 1)
  taiw.ts<- ts(data$taiw,start = c(1950,1),frequency = 1)
  hongk.ts<- ts(data$hongk,start = c(1950,1),frequency = 1)
  sing.ts<- ts(data$sing,start = c(1950,1),frequency = 1)
  jpn.ts<- ts(data$jpn,start = c(1950,1),frequency = 1)
  
# Section 4 ---------------------------------------------------------------
  summary(ur.df(col.ts, type = 'trend', lags = 4))
  
# Section 5 ---------------------------------------------------------------
  ## ADF
    adf.t<- function(x){
      for (i in -8:-1) {
        if(abs(ur.df(x, 
                     type="trend", 
                     lag=-i)@testreg$coefficients[-i+3,3]) >= 1.60){
        break
            
        }  
      }
    ### Elaboramos los resultados
      df<- data.frame(rowname = "",
                      summary(ur.df(x, 
                                  type="trend", 
                                  lag=-i))@testreg$coefficients[2,1],
                      summary(ur.df(x, 
                                  type="trend", 
                                  lag=-i))@teststat[1],
                      summary(ur.df(x, 
                                  type="trend", 
                                  lag=-i))@cval[1],
                      summary(ur.df(x, 
                                  type="trend", 
                                  lag=-i))@cval[4],
                      summary(ur.df(x, 
                                  type="trend", 
                                  lag=-i))@cval[7],
                      -i,
                      summary(ur.df(x, 
                                  type="trend", 
                                  lag=-i))@testreg$coefficients[-i+3,3],
                      -(summary(ur.df(x, 
                                    type="trend", 
                                    lag=-i))@testreg$coefficients[1,1]/summary(ur.df(x, 
                                                                                     type="trend", 
                                                                                     lag=-i))@testreg$coefficients[2,1]))
      colnames(df)<- c("",
                       "alpha",
                       "t-alpha",
                       "1%",
                       "5%",
                       "10%",
                       "Lag",
                       "t-lag",
                       "Mean")
      format(df,
             scientific = FALSE, 
             digits = 3)
    }
# Section 6 ---------------------------------------------------------------
  ## Elaboramos una funcion que aplique el DFGLS
    ers<- function(x){
        for (i in -8:-1) {
            if(abs(ur.ers(x, 
                          model="constant", 
                          lag.max=-i)@testreg$coefficients[-i+1,3])>=1.60){
                break
                
            }  
            
        }
                
        ### Determinamos si se encuentra en una TIM
        if (ti<= mean(x) & mean(x)<= ts & summary(ur.ers(x, 
                                                         model="constant", 
                                                         lag.max=-i))@teststat<=summary(ur.ers(x, 
                                                                                               model="constant", 
                                                                                               lag.max=-i))@cval[3]) { 
            Ex <- "MIT"
        } else{
            Ex <- "-"
        }
        
        ### Elaboramos el cuadro de resultados
        df<- data.frame(rowname="",
                        summary(ur.ers(x, 
                                       model="constant", 
                                       lag.max=-i))@testreg$coefficients[1,1],
                        summary(ur.ers(x, 
                                       model="constant", 
                                       lag.max=-i))@teststat, 
                        summary(ur.ers(x, 
                                       model="constant", 
                                       lag.max=-i))@cval[1], 
                        summary(ur.ers(x, 
                                       model="constant", 
                                       lag.max=-i))@cval[2], 
                        summary(ur.ers(x, 
                                       model="constant", 
                                       lag.max=-i))@cval[3],-i,
                        summary(ur.ers(x, 
                                       model="constant", 
                                       lag.max=-i))@testreg$coefficients[-i+1,3], 
                        mean(x), 
                        Ex)
        colnames(df)<- c("",
                         "alpha",
                         "t-alpha",
                         "1%",
                         "5%",
                         "10%",
                         "Lag",
                         "t-Lag", 
                         "Mean", 
                         "?MIT?")
        format(df, 
               scientific = FALSE, 
               digits = 3)
    }
# Section 7 ----------------------------------------------------------------
  ## Aplicamos las funciones a los paises 
    tests<- c('adf.t', 'ers')
    for (i in tests) {
        x <- apply(data[1:9], 
                   MARGIN = 2, 
                   i) 
        print(x)
    }

