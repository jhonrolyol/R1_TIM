#===============================================================================
#                         TREND ASSESSMENT
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
                  range = "A4:AH72")
# Section 2 ---------------------------------------------------------------
  ## Niveles de significancia
    alpha1<- 0.01
    alpha5<- 0.05
    alpha10<- 0.10
    ti<- mean(ts(data[,'linf']))
    ts<- mean(ts(data[,'lsup']))
    
#============================================
# SIGNIFICANCIA DE LA TENDENCIA POST QUIEBRE -----------------------------------
#============================================    
    
# Section 3 ---------------------------------------------------------------
    
#=======
# CHILE ------------------------------------------------------------------------
#=======
  chil.ts <- ts(data$chil,
               start = c(1950,1),
               frequency = 1)
  tb <- 36 # punto de quiebre
  N <- length(chil.ts)
  k <- 6 # orden del rezago segun Perron 

  ## Testeamos mediante el adf y evaluamos la significancia 
  ## de la tendencia post quiebre
    chil.df<- ur.df(chil.ts[37:N],type="trend",lag=k)
    summary(chil.df)

  ## Generamos valores criticos bootstrap con 5000 submuestras
    hrz<- 5000 # numero de submuestras
    data.boot<- matrix(nrow=N-tb,
                       ncol = hrz) # generamos una matriz donde cada columna 
                                   #  tenga cada submuestra
  ## generamos un vector con todos los t de la tendencia 
  ## de las 5000 submuestras
    tvalue<- matrix(nrow=hrz,
                    ncol=1) 
    for (i in 1:hrz){
      data.boot[,i]<- sample(chil.ts[37:N],
                             N-tb,
                             replace=TRUE)
      tvalue[i]<- ur.df(data.boot[,i],
                        type="trend",
                        lag=k)@testreg$coefficients[3,"t value"]
    }
    plot(density(tvalue))
    
  ## Valores criticos al 1%, 5% y 10%
    chil.li1<- quantile(tvalue,
                        probs = alpha1/2) 
    chil.ls1<- quantile(tvalue,
                        probs = 1-alpha1/2)
    
    chil.li5<- quantile(tvalue,
                        probs = alpha5/2) 
    chil.ls5<- quantile(tvalue,
                        probs = 1-alpha5/2)
    
    chil.li10<- quantile(tvalue,
                         probs = alpha10/2) 
    chil.ls10<- quantile(tvalue,
                         probs = 1-alpha10/2)
    
    df.chil1<- data.frame(chil.li1, 
                          chil.ls1, 
                          chil.li5, 
                          chil.ls5, 
                          chil.li10, 
                          chil.ls10)
    format(df.chil1,
           scientific = FALSE, 
           digits = 3)
# Section 4 ---------------------------------------------------------------

#==========
# COLOMBIA ---------------------------------------------------------------------
#==========
  col.ts<- ts(data$col,
              start = c(1950,1),
              frequency = 1)

  ## ADF
    k<- 4
    N<- length(col.ts)
  ## Testeamos mediante el adf y evaluamos la significancia 
  ## de la tendencia post quiebre
    col.df<- ur.df(col.ts,type="trend",lag=k)
    summary(col.df)

  ## Generamos valores criticos bootstrap con 5000 submuestras
    hrz<- 5000 #numero de submuestras
    data.boot<- matrix(nrow=N,
                       ncol=hrz) # generamos una matriz donde cada
                                 # columna tenga cada submuestra
  ## generamos un vector con todos los t de la tendencia 
  ## de las 5000 submuestras
    tvalue<- matrix(nrow=hrz,
                    ncol=1) 
    for (i in 1:hrz){
        data.boot[,i]<- sample(col.ts,
                               N,
                               replace=TRUE)
        tvalue[i]<- ur.df(data.boot[,i],
                          type="trend",
                          lag=k)@testreg$coefficients[3,"t value"]
    }
    windows()
    plot(density(tvalue))
  ## Valores criticos al 1%, 5% y 10%
    col.li1<- quantile(tvalue,
                       probs = alpha1/2) 
    col.ls1<- quantile(tvalue,
                       probs = 1-alpha1/2)
    
    col.li5<- quantile(tvalue,
                       probs = alpha5/2) 
    col.ls5<- quantile(tvalue,
                       probs = 1-alpha5/2)
    
    col.li10<- quantile(tvalue,
                        probs = alpha10/2) 
    col.ls10<- quantile(tvalue,
                        probs = 1-alpha10/2)
    
    df.col1<- data.frame(col.li1, 
                         col.ls1, 
                         col.li5, 
                         col.ls5, 
                         col.li10, 
                         col.ls10)
    format(df.col1,scientific = FALSE, 
           digits = 3)
  ## ZA(C)
    tb<- 48 # punto de quiebre
    N<- length(col.ts)
    k<- 1 #orden del rezago segun Perron

  ## Testeamos mediante el adf y evaluamos la significancia 
  ## de la tendencia post quiebre
    col.df<- ur.df(col.ts[49:N],
                   type = "trend",
                   lag = k)
    summary(col.df)
  ## Generamos valores criticos bootstrap con 5000 submuestras
    hrz<- 5000 #numero de submuestras
    data.boot<- matrix(nrow=N-tb,ncol =hrz) # generamos una matriz donde cada 
                                            # columna tenga cada submuestra

  ## generamos un vector con todos los t de la tendencia 
  ## de las 5000 submuestras
    tvalue<- matrix(nrow=hrz,ncol=1) 
    for (i in 1:hrz){
        data.boot[,i]<- sample(col.ts[49:N],
                               N-tb,
                               replace=TRUE)
        tvalue[i]<- ur.df(data.boot[,i],
                          type="trend",
                          lag=k)@testreg$coefficients[3,"t value"]
    }
    plot(density(tvalue))
  ## Valores criticos al 1%, 5% y 10%
    col.li1<- quantile(tvalue,
                       probs = alpha1/2) 
    col.ls1<- quantile(tvalue,
                       probs = 1-alpha1/2)
    
    col.li5<- quantile(tvalue,
                       probs = alpha5/2) 
    col.ls5<- quantile(tvalue,
                       probs = 1-alpha5/2)
    
    col.li10<- quantile(tvalue,
                        probs = alpha10/2) 
    col.ls10<- quantile(tvalue,
                        probs = 1-alpha10/2)



# Section 5 ---------------------------------------------------------------

#========
# MEXICO -----------------------------------------------------------------------
#========    
    
  mx.ts<- ts(data$mx,
             start = c(1950,1),
             frequency = 1)
  tb<- 45 # segundo punto de quiebre
  N<- length(mx.ts)
  k<- 5 # orden del rezago segun Perron 
  ## Testeamos mediante el adf y evaluamos la significancia 
  ## de la tendencia post quiebre
    mx.df<- ur.df(mx.ts[45:N],type="trend",lag=k)
    summary(mx.df)
  ## Generamos valores criticos bootstrap con 5000 submuestras
  hrz<- 5000 # numero de submuestras
  data.boot<- matrix(nrow=N-tb,
                     ncol = hrz) # generamos una matriz donde cada 
                                 # columna tenga cada submuestra
  
  ## generamos un vector con todos los t de la tendencia 
  ## de las 5000 submuestras
    tvalue<- matrix(nrow=hrz,ncol=1) 
    for (i in 1:hrz){
        data.boot[,i]<- sample(mx.ts[46:N],
                               N-tb,
                               replace=TRUE)
        tvalue[i]<- ur.df(data.boot[,i],
                          type="trend",
                          lag=k)@testreg$coefficients[3,"t value"]
    }
    plot(density(tvalue))
  ## Valores criticos al 1%, 5% y 10%
    mx.li1<- quantile(tvalue,
                      probs = alpha1/2) 
    mx.ls1<- quantile(tvalue,
                      probs = 1-alpha1/2)
    
    mx.li5<- quantile(tvalue,
                      probs = alpha5/2) 
    mx.ls5<- quantile(tvalue,
                      probs = 1-alpha5/2)
    
    mx.li10<- quantile(tvalue,
                       probs = alpha10/2) 
    mx.ls10<- quantile(tvalue,
                       probs = 1-alpha10/2)

# Section 6 ---------------------------------------------------------------
    
#===============
# COREA DEL SUR ----------------------------------------------------------------
#=============== 
    
  cor.ts<- ts(data$cor,
              start = c(1950,1),
              frequency = 1)
  tb<- 52 # punto de quiebre
  N<- length(cor.ts)
  k<- 3 # orden del rezago segun Perron
  ## Testeamos mediante el adf y evaluamos la significancia 
  ## de la tendencia post quiebre
    cor.df<- ur.df(cor.ts[53:N],
                   type="trend",
                   lag=k)
    summary(cor.df)

  ## Generamos valores criticos bootstrap con 5000 submuestras
    hrz<- 5000 #numero de submuestras
    data.boot<- matrix(nrow=N-tb,
                       ncol =hrz) # generamos una matriz donde cada columna
                                  # tenga cada submuestra

  ## generamos un vector con todos los t de la tendencia 
  ## de las 5000 submuestras
  tvalue<- matrix(nrow=hrz,
                  ncol=1) 
  for (i in 1:hrz){
      data.boot[,i]<- sample(cor.ts[53:N],
                             N-tb,
                             replace=TRUE)
      tvalue[i]<- ur.df(data.boot[,i],
                        type="trend",
                        lag=k)@testreg$coefficients[3,"t value"]
  }
  plot(density(tvalue))

  ## Valores criticos al 1%, 5% y 10%
    cor.li1<- quantile(tvalue,
                       probs = alpha1/2) 
    cor.ls1<- quantile(tvalue,
                       probs = 1-alpha1/2)
    
    cor.li5<- quantile(tvalue,
                       probs = alpha5/2) 
    cor.ls5<- quantile(tvalue,
                       probs = 1-alpha5/2)
    
    cor.li10<- quantile(tvalue,
                        probs = alpha10/2) 
    cor.ls10<- quantile(tvalue,
                        probs = 1-alpha10/2)

    
# Section 7 ---------------------------------------------------------------
    
#========
# TAIWAN -----------------------------------------------------------------------
#========   
  taiw.ts<- ts(data$taiw,
               start = c(1950,1),
               frequency = 1)
  tb<- 44 # punto de quiebre
  N<- length(taiw.ts)
  k<- 8 # orden del rezago segun Perron
  ## Testeamos mediante el adf y evaluamos la significancia 
  ## de la tendencia post quiebre
  taiw.df<- ur.df(taiw.ts[45:N],
                  type="trend",
                  lag=k)
  summary(taiw.df)
  ## Generamos valores criticos bootstrap con 5000 submuestras
    hrz<- 5000 # numero de submuestras
    data.boot<- matrix(nrow=N-tb,
                       ncol = hrz) # generamos una matriz donde cada columna
                                   # tenga cada submuestra
  
  ## generamos un vector con todos los t de la tendencia 
  ## de las 5000 submuestras
  tvalue<- matrix(nrow=hrz,
                  ncol=1) 
  for (i in 1:hrz){
      data.boot[,i]<- sample(taiw.ts[45:N],
                             N-tb,
                             replace=TRUE)
      tvalue[i]<- ur.df(data.boot[,i],
                        type="trend",
                        lag=k)@testreg$coefficients[3,"t value"]
  }
  plot(density(tvalue))
  ## Valores criticos al 1%, 5% y 10%
    taiw.li1<- quantile(tvalue,
                        probs = alpha1/2) 
    taiw.ls1<- quantile(tvalue,
                        probs = 1-alpha1/2)
    
    taiw.li5<- quantile(tvalue,
                        probs = alpha5/2) 
    taiw.ls5<- quantile(tvalue,
                        probs = 1-alpha5/2)
    
    taiw.li10<- quantile(tvalue,
                         probs = alpha10/2) 
    taiw.ls10<- quantile(tvalue,
                         probs = 1-alpha10/2)
# Section 8 ---------------------------------------------------------------

#==========
# SINGAPUR ---------------------------------------------------------------------
#==========
  sing.ts<- ts(data$sing,
               start = c(1950,1),
               frequency = 1)
  tb<- 19 # punto de quiebre
  N<- length(sing.ts)
  k<- 1 # orden del rezago segun Perron
  ## Testeamos mediante el adf y evaluamos la significancia 
  ## de la tendencia post quiebre
  sing.df<- ur.df(sing.ts[20:N],
                  type="trend",
                  lag=k)
  summary(sing.df)
  ## Generamos valores criticos bootstrap con 5000 submuestras
    hrz<- 5000 # numero de submuestras
    data.boot<- matrix(nrow=N-tb,
                       ncol =hrz) # generamos una matriz donde cada columna
                                  # tenga cada submuestra

  ## generamos un vector con todos los t de la tendencia 
  ## de las 5000 submuestras
    tvalue<- matrix(nrow=hrz,
                    ncol=1) 
    for (i in 1:hrz){
        data.boot[,i]<- sample(sing.ts[20:N],
                               N-tb,
                               replace=TRUE)
        tvalue[i]<- ur.df(data.boot[,i],
                          type="trend",
                          lag=k)@testreg$coefficients[3,"t value"]
    }
    plot(density(tvalue))


  ## Valores criticos al 1%, 5% y 10%
    sing.li1<- quantile(tvalue,
                        probs = alpha1/2) 
    sing.ls1<- quantile(tvalue,
                        probs = 1-alpha1/2)
    
    sing.li5<- quantile(tvalue,
                        probs = alpha5/2) 
    sing.ls5<- quantile(tvalue,
                        probs = 1-alpha5/2)
    
    sing.li10<- quantile(tvalue,
                         probs = alpha10/2) 
    sing.ls10<- quantile(tvalue,
                         probs = 1-alpha10/2)


        