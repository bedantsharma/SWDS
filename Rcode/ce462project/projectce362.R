require("pacman")
p_load(reshape2 , plotly)
p_load("ggplot2","ggraph","rio","httr","ggvis","tidyr","fitdistrplus","evd","extRemes")
#part A of PS
{
  ##importing data
  {
    data <- import("/Users/bedantsharma/Desktop/desktop/sem6/ce462/210260/Rcode/dataiitk.csv")
    summary(data)
    data <- data[-1,]
  }
  ## data separation
  {
    nrows <- length(data$V1)
    ncol <- length(data[1,])
    years <- data$V1[2:nrows]
    a_min <- as.numeric(data$V2[2:nrows])
    b_min <- as.numeric(data$V3[2:nrows])
    c_min <- as.numeric(data$V4[2:nrows])
    d_min <- as.numeric(data$V5[2:nrows])
    e_min <- as.numeric(data$V6[2:nrows])
    f_min <- as.numeric(data$V7[2:nrows])
    g_min <- as.numeric(data$V8[2:nrows])
    h_min <- as.numeric(data$V9[2:nrows])
    i_min <- as.numeric(data$V10[2:nrows])
  }
  # raw data plots 
  {plot(x <- seq(1, 10, by = 0.1),3*sin(x) , main = "just to fill the current ploting area",
        xlab = "x" , ylab = "sin(x)" , type = "l") 
    plotrawdata <- function(){
      dev.new()
      par(mfrow = c(3,3))
      hist(as.numeric(a_min), breaks = 15, main = "histogram of a_min data")
      hist(as.numeric(b_min), breaks = 15, main = "Histogram of b_min data")
      hist(as.numeric(c_min), breaks = 15, main = "Histogram of c_min data")
      hist(as.numeric(d_min), breaks = 15, main = "Histogram of d_min data")
      hist(as.numeric(e_min), breaks = 15, main = "Histogram of e_min data")
      hist(as.numeric(f_min), breaks = 15, main = "Histogram of f_min data")
      hist(as.numeric(g_min), breaks = 15, main = "Histogram of g_min data")
      hist(as.numeric(h_min), breaks = 15, main = "Histogram of h_min data")
      hist(as.numeric(i_min), breaks = 15, main = "Histogram of i_min data")
      par(mfrow = c(1,1))
    }
    plotrawdata()
    #ploting boxplot of the data
    {
      dev.new()
      par(mfrow = c(3,3))
      boxplot(as.numeric(a_min), main = "Boxplot of a_min data")
      boxplot(as.numeric(b_min), main = "Boxplot of b_min data")
      boxplot(as.numeric(c_min), main = "Boxplot of c_min data")
      boxplot(as.numeric(d_min), main = "Boxplot of d_min data")
      boxplot(as.numeric(e_min), main = "Boxplot of e_min data")
      boxplot(as.numeric(f_min), main = "Boxplot of f_min data")
      boxplot(as.numeric(g_min), main = "Boxplot of g_min data")
      boxplot(as.numeric(h_min), main = "Boxplot of h_min data")
      boxplot(as.numeric(i_min), main = "Boxplot of i_min data")
      par(mfrow = c(1,1))
      
      
    }
  }
  
  #removing outliers 
  {
    a_out_rm <- a_min[!a_min %in% boxplot.stats(a_min)$out]
    b_out_rm <- b_min[!b_min %in% boxplot.stats(b_min)$out]
    c_out_rm <- c_min[!c_min %in% boxplot.stats(c_min)$out]
    d_out_rm <- d_min[!d_min %in% boxplot.stats(d_min)$out]
    e_out_rm <- e_min[!e_min %in% boxplot.stats(e_min)$out]
    f_out_rm <- f_min[!f_min %in% boxplot.stats(f_min)$out]
    g_out_rm <- g_min[!g_min %in% boxplot.stats(g_min)$out]
    h_out_rm <- h_min[!h_min %in% boxplot.stats(h_min)$out]
    i_out_rm <- i_min[!i_min %in% boxplot.stats(i_min)$out]
 }
  {
    a_min <- a_out_rm
    b_min <- b_out_rm
    c_min <- c_out_rm
    d_min <- d_out_rm
    e_min <- e_out_rm
    f_min <- f_out_rm
    g_min <- g_out_rm
    h_min <- h_out_rm
    i_min <- i_out_rm
    
  }
  {
    rm(a_out_rm)
    rm(b_out_rm)
    rm(c_out_rm)
    rm(d_out_rm)
    rm(e_out_rm)
    rm(f_out_rm)
    rm(g_out_rm)
    rm(h_out_rm)
    rm(i_out_rm)
    
}
  ## fitting data to Log-Normal and Gumbel’s distribution
  # gumbel distribution
  {
    gumbel_a <- fevd(as.numeric(a_min), type = "Gumbel")
    gumbel_b <- fevd(as.numeric(b_min), type = "Gumbel")
    gumbel_c <- fevd(as.numeric(c_min), type = "Gumbel")
    gumbel_d <- fevd(as.numeric(d_min), type = "Gumbel")
    gumbel_e <- fevd(as.numeric(e_min), type = "Gumbel")
    gumbel_f <- fevd(as.numeric(f_min), type = "Gumbel")
    gumbel_g <- fevd(as.numeric(g_min), type = "Gumbel")
    gumbel_h <- fevd(as.numeric(h_min), type = "Gumbel")
    gumbel_i <- fevd(as.numeric(i_min), type = "Gumbel")
  }
  # log normal distribution 
  {
    lnorm_a <- fitdist(as.numeric(a_min), "lnorm")
    lnorm_b <- fitdist(as.numeric(b_min), "lnorm")
    lnorm_c <- fitdist(as.numeric(c_min), "lnorm")
    lnorm_d <- fitdist(as.numeric(d_min), "lnorm")
    lnorm_e <- fitdist(as.numeric(e_min), "lnorm")
    lnorm_f <- fitdist(as.numeric(f_min), "lnorm")
    lnorm_g <- fitdist(as.numeric(g_min), "lnorm")
    lnorm_h <- fitdist(as.numeric(h_min), "lnorm")
    lnorm_i <- fitdist(as.numeric(i_min), "lnorm")
  }
  # comparing log normal to Gumbel to find out which is better for which time period 
  {
    comparing_fun <- function(lnorm,gumbel){
      
    }
    # a = 5 min
    summary(gumbel_a)
    summary(lnorm_a)
    # Gumbel is better 
    
    # b = 10 min 
    summary(gumbel_b) 
    summary(lnorm_b)
    # lnorm is better
    
    # c = 15 min 
    summary(gumbel_c) 
    summary(lnorm_c)
    # lnorm is better
    
    
    # d = 30 min
    summary(gumbel_d) 
    summary(lnorm_d)
    # lnorm is better
    
    
    # e = 1 hr
    summary(gumbel_e) 
    summary(lnorm_e)
    # lnorm is better
    
    
    # f = 2hr
    summary(gumbel_f) 
    summary(lnorm_f)
    # gumbel is better
    
    
    # g = 6hr
    summary(gumbel_g) 
    summary(lnorm_g)
    # lnorm is better
    
    
    # h = 12hr
    summary(gumbel_h) 
    summary(lnorm_h)
    # lnorm is better
    
    
    # i = 24hr
    summary(gumbel_i) 
    summary(lnorm_i)
    # lnorm is better
  }
  # calculation of Fx for give return period - 
  {
    T <- c(2, 5, 10, 15, 30 ,50)
    Fx <- 1-(1/T)
    
    
    calc_i_gumbel <- function(Fx, distribution) {
      qgumbel(Fx, loc = as.numeric(distribution$results$par[1]), 
              scale = as.numeric(distribution$results$par[2]))
    }
    
    calc_i_lnorm <- function(Fx, distribution){
      qlnorm(Fx,meanlog = as.numeric(distribution$estimate[1]) ,
             sdlog = as.numeric(distribution$estimate[2]))
    }
  }
  # calculating intensity for various td's 
  {
    # a and f ki liye gumbel use karna hai 
    i_a <- calc_i_gumbel(Fx,gumbel_a)
    i_b <- calc_i_lnorm(Fx,lnorm_b)
    i_c <- calc_i_lnorm(Fx,lnorm_c)
    i_d <- calc_i_lnorm(Fx,lnorm_d)
    i_e <- calc_i_lnorm(Fx,lnorm_e)
    i_f <- calc_i_gumbel(Fx,gumbel_f)
    i_g <- calc_i_lnorm(Fx,lnorm_g)
    i_h <- calc_i_lnorm(Fx,lnorm_h)
    i_i <- calc_i_lnorm(Fx,lnorm_i)
    ## making a dataframe of these i__'s
    idf_data <- data.frame(
      Return_Period = T,
      I_a = i_a,
      I_b = i_b,
      I_c = i_c,
      I_d = i_d,
      I_e = i_e,
      I_f = i_f,
      I_g = i_g,
      I_h = i_h,
      I_i = i_i
    )
  }
  #converting i__'s to there respective time intervals
  {
    t_d <- c(
      as.difftime(5, units = "mins"),
      as.difftime(10, units = "mins"),
      as.difftime(15, units = "mins"),
      as.difftime(30, units = "mins"),
      as.difftime(1, units = "hours"),
      as.difftime(2, units = "hours"),
      as.difftime(6, units = "hours"),
      as.difftime(12, units = "hours"),
      as.difftime(24, units = "hours")
    )
  }
  convert_i <- function(var){
    if(var == "I_a"){
      return(as.character(t_d[1]))
    }
    else if (var == "I_b"){
      return(as.character(t_d[2]))
    }
    else if (var == "I_c"){
      return(as.character(t_d[3]))
    }
    else if (var == "I_d"){
      return(as.character(t_d[4]))
    }
    else if (var == "I_e"){
      return(as.character(t_d[5]))
    }
    else if (var == "I_f"){
      return(as.character(t_d[6]))
    }
    else if (var == "I_g"){
      return(as.character(t_d[7]))
    }
    else if (var == "I_h"){
      return(as.character(t_d[8]))
    }
    return(as.character(t_d[9]))
    
  } # if needed 
  # i ended up not using this convert_i function
  
  # ploting the idf curve
  {
    dev.new()
    #
    plot(as.numeric(t_d)/60, idf_data[6, 2:10], col = "#1FFFA3", type = "l",
         xlab = "t_d in min (log scale)", ylab = "intensity", log = "x",
         main = "idf curve in semi log scale")
    lines(as.numeric(t_d)/60, idf_data[2, 2:10], col = "#FFAF33")
    lines(as.numeric(t_d)/60, idf_data[3, 2:10], col = "#BBFF33")
    lines(as.numeric(t_d)/60, idf_data[4, 2:10], col = "#3CFF33")
    lines(as.numeric(t_d)/60, idf_data[5, 2:10], col = "#1789AE")
    lines(as.numeric(t_d)/60, idf_data[1, 2:10], col = "#FA5999")
    points(as.numeric(t_d)/60, idf_data[6, 2:10], col = "#1FFFA3")
    points(as.numeric(t_d)/60, idf_data[1, 2:10], col = "#FA5999",)
    points(as.numeric(t_d)/60, idf_data[2, 2:10], col = "#FFAF33")
    points(as.numeric(t_d)/60, idf_data[3, 2:10], col = "#BBFF33")
    points(as.numeric(t_d)/60, idf_data[4, 2:10], col = "#3CFF33")
    points(as.numeric(t_d)/60, idf_data[5, 2:10], col = "#1789AE")
    
    # Add legend at the top right
    legend("topleft", legend = c("2 year", "5 year", "10 year", "15 year", "30 year", "50 year"),
           col = c("#FF5733", "#FFAF33", "#BBFF33", "#3CFF33", "#1789AE", "#BAAF36"),
           lty = 1, cex = 0.8)
    # Add grid lines
    #grid(lty = 1, col = "black")
  }
  
  write.csv(idf_data,"/Users/bedantsharma/Desktop/desktop/sem6/ce462/210260/Rcode/idf.csv",row.names = FALSE)
  
  #fitting the given equation to the idf data
  {
    data_ <- import("/Users/bedantsharma/Desktop/desktop/sem6/ce462/210260/Rcode/idf_new.csv")
    # x = [T,D]
    data_long <- melt(data_, id.vars = "Return_Period" , 
                      variable.name = "time_interval" , 
                      value.name = "intensity")
    head(data_long)
    # parm = [k,x,a,n]
    objective_function <- function(parm){
      k <- parm[1]
      x <- parm[2]
      a <- parm[3]
      n <- parm[4]
      i_td = (k*(Ti^x))/((Di+a)^n)
      
      loss <- sum( (i_td - as.vector(data_long$intensity))^2 )
      
      return(loss)
    }
    
    Ti <- data_long$Return_Period
    Di <- as.numeric(as.vector(data_long$time_interval))
    
    initial_guess = c(1,1,1,1)
    
    res <- optim(par = initial_guess , 
                 fn = objective_function, 
                 method = "BFGS")
  }
  # setting parameters and creating the intensity function
  {
    parm <- res$par
    k <- parm[1]
    x <- parm[2]
    a <- parm[3]
    n <- parm[4]
    i_td <- function(return_period , time_interval){
      return((k*(return_period^x))/(( time_interval + a)^n))
    }
  }

  ##trying to plot interval vs intensity for 
  ##different time periods intensity for better visualization
  
  {
    rt_2 <- i_td(return_period = rep(2,times = 9), time_interval = as.numeric(t_d))
    rt_5 <- i_td(return_period = rep(5,times = 9), time_interval = as.numeric(t_d))
    
    dev.new()
    
    plot(as.numeric(t_d)/60,rt_5,col = "#ffa33e",
         xlab = "interval of strom(min)",
         ylab = "intensity",
         main = "real vs estimated idf curve",
         type = "l",
         log = "x")
    lines(as.numeric(t_d)/60,rt_2,col = "#ffa33e")
    lines(as.numeric(t_d)/60, idf_data[1, 2:10], col = "black")
    lines(as.numeric(t_d)/60, idf_data[2, 2:10], col = "black")
    points(as.numeric(t_d)/60,rt_2,col = "#ffa33e")
    points(as.numeric(t_d)/60,rt_5,col = "#ffa33e")
    points(as.numeric(t_d)/60, idf_data[1, 2:10], col = "black")
    points(as.numeric(t_d)/60, idf_data[2, 2:10], col = "black")
    legend("topleft",legend = c("true idf curve","estimated curve for the equation"),
           col = c("black","#ffa33e"),lty = 1)
  }
}
# part B of PS
{
  # assuming hot mix asphalt and concrete 
  
  ti_data <- import("/Users/bedantsharma/Desktop/desktop/sem6/ce462/210260/Rcode/kerbyRandlength.csv")
  area_data <- import("/Users/bedantsharma/Desktop/desktop/sem6/ce462/210260/Rcode/areaOfBasin.csv")
  Rl_data <- import("/Users/bedantsharma/Desktop/desktop/sem6/ce462/210260/Rcode/RL_of_manhole.csv")
  Rl_data <- Rl_data[-9,]
  length_slope_data <- import("/Users/bedantsharma/Desktop/desktop/sem6/ce462/210260/Rcode/slope_length_pipeline.csv")
  manning_n <- 0.015
  rt_pr <- 5
  circular_pipe_coff <- 3.20
  area <- area_data$areaOfBasin*0.01 # in km^2
  length_of_basin <- ti_data$`drainage length (m)`
  slope_of_basin <- ti_data$`slope of the basin`
  KerbyR <- ti_data$`kerby's cofficient`
  
  ti_of_each_basin <- 1.44*(length_of_basin*KerbyR/slope_of_basin^1/2)^0.467
  #assuming runoff coefficients 0.15 lawn 0.6 residential 0.80 street
  C_ <- c(0.6,0.15,0.80)
  c_of_each_basin <- NA
  for (i in 1:length(area_data[,1])) {
    c_of_each_basin <- c(c_of_each_basin,sum(C_*area_data[i,3:5]))
  }
  c_of_each_basin <- c_of_each_basin[-1]
  c_of_each_basin <- c_of_each_basin/100
  
  circle_area <- function(dia) {
    area <- (pi * dia^2) / 4
    return(area)
  }
  
  dia_of_pipe <- function(Q,s0){
    
    cons <- manning_n*3.208/sqrt(s0)
    ans <- (cons*Q)^(3/8)
    return(ans)
  }
  
  # manhole 11
  ti11 = ti_of_each_basin[1]
  i11 <- i_td(5,ti11*60)
  c1a1 <- c_of_each_basin[1]*area[1]
  Q11 <- c1a1*i11/3.6
  d11 <- dia_of_pipe(Q11,length_slope_data$`avg. slope`[1])
  
  
  # manhole 21
  v11 <- Q11/circle_area(d11)
  t_arrival21 <- (length_slope_data$`length of pipeline`[1]/v11)/60
  
  ti21 <- max(c(ti_of_each_basin[2],t_arrival21+ti11))
  i21 <- i_td(5,ti21*60)
  c2a2 <- c_of_each_basin[2]*area[2]
  Q21 <- c2a2*i21/3.6
  d21 <- dia_of_pipe(Q21,length_slope_data$`avg. slope`[2])
  
  #manhole 31
  v21 <- Q21/circle_area(d21)
  t_arrival31 <- (length_slope_data$`length of pipeline`[2]/v21)/60
  
  ti31 <- max(c(ti_of_each_basin[3],t_arrival31+ti21))
  i31 <- i_td(5,ti31*60)
  c3a3 <- c_of_each_basin[3]*area[3]
  Q31 <- c3a3*i31/3.6
  d31 <- dia_of_pipe(Q31,length_slope_data$`avg. slope`[3])
  
  #manhole 41
  v31 <- Q31/circle_area(d31)
  t_arrival41 <- (length_slope_data$`length of pipeline`[3]/v31)/60
  
  ti41 <- max(c(ti_of_each_basin[4],t_arrival41+ti31))
  i41 <- i_td(5,ti41*60)
  c4a4 <- c_of_each_basin[4]*area[4]
  Q41 <- c4a4*i41/3.6
  d41 <- dia_of_pipe(Q41,length_slope_data$`avg. slope`[4])
  
  #manhole 51
  v41 <- Q41/circle_area(d41)
  t_arrival51 <- (length_slope_data$`length of pipeline`[4]/v41)/60
  
  ti51 <- max(c(ti_of_each_basin[5],t_arrival51+ti41))
  i51 <- i_td(5,ti51*60)
  c5a5 <- c_of_each_basin[5]*area[5]
  Q51 <- c5a5*i51/3.6
  d51 <- dia_of_pipe(Q51,length_slope_data$`avg. slope`[5])
  
  #manhole 61
  
  v51 <- Q51/circle_area(d51)
  t_arrival61 <- (length_slope_data$`length of pipeline`[5]/v51)/60
  
  ti61 <- max(c(ti_of_each_basin[6],t_arrival61+ti51))
  i61 <- i_td(5,ti61*60)
  c6a6 <- c_of_each_basin[6]*area[6]
  Q61 <- c6a6*i61/3.6
  d61 <- dia_of_pipe(Q61,length_slope_data$`avg. slope`[6])
  
  #manhole 16
  ti16 <- ti_of_each_basin[6]
  i16 <- i_td(5,ti16*60)
  c6a6 <- c_of_each_basin[6]*area[6]
  Q16 <- c6a6*i16/3.6
  d16 <- dia_of_pipe(Q16,length_slope_data$`avg. slope`[6])
  
  #manhole 17
  ti17 <- ti_of_each_basin[7]
  i17 <- i_td(5,ti17*60)
  c7a7 <- c_of_each_basin[7]*area[7]
  Q17 <- c7a7*i17/3.6
  d17 <- dia_of_pipe(Q17,length_slope_data$`avg. slope`[7])
  
  # therefore non of the diameter is greater the 0.45 hence taking all the diamenter
  # to be 0.45
  
  slll <- length_slope_data$`length of pipeline`*length_slope_data$`avg. slope`
  upcl <- Rl_data$RL - 1
  upcl <- upcl[-6]
  dscl <- upcl - slll
  usil <- upcl - dia_of_each_pipe
  dsil <- dscl - dia_of_each_pipe
   
  name_of_each_pipe <- c('11','21','31','41','51','16','17')
  
  #exporting data
  
  export1 <- data.frame(name_of_each_pipe,slll,upcl,dscl,usil,dsil)
  
  write.csv(export1,"/Users/bedantsharma/Desktop/desktop/sem6/ce462/210260/Rcode/elevation_data_of_each_pipe.csv")
  Q_of_each_manhole <- c(Q11,Q21,Q31,Q41,Q51,Q61,Q16,Q17)
  ti_of_each_manhole <- c(ti11,ti21,ti31,ti41,ti51,ti61,ti16,ti17)
  write.csv(data.frame(Q_of_each_manhole,ti_of_each_manhole),"/Users/bedantsharma/Desktop/desktop/sem6/ce462/210260/Rcode/qiti.csv")
  #calculating the spread of gutter
  sl_l <- 0.001
  sx_x <- 0.025
  
  gutter_constant__ <- 590.2057
  
  spread_of_gutter <- (gutter_constant__*Q_of_each_manhole)^3/8
  
  
}
#part C of PS
{
  time_int <- 12*60*60
  req_i_value <- i_td(return_period = 100,time_interval = time_int)
  print(req_i_value)
}
# part D of ps
{
  
  # 1.5hr storm 25 year return_period 15 min time interval
  
  ret_p <- 25
  time_int <- 15*60
  duration <- 1.5*60*60
  
  time_ser <- seq(0,duration,by = time_int)
  
  i_dd <- i_td(return_period = rep(ret_p,times = length(time_ser)) , time_interval = time_ser)
  i_dd <- i_dd[-1]
  i_dd <- c(0,i_dd)
  cumelative_rainfall <- i_dd*time_ser/3600
  inc_depth <- NA
  for (i in 2:length(cumelative_rainfall)) {
    inc_depth <- c(inc_depth,cumelative_rainfall[i]-cumelative_rainfall[i-1])
  }
  inc_depth <- inc_depth[-1]
  
  rearr <- function(arr) {
    arr <- sort(arr,decreasing = F)
    n <- length(arr)
    ans <- vector(mode = "numeric", length = n)
    ans[(n+1)/2] <- tail(arr, 1)
    p <- n - 1
    r <- (n+1)/2 + 1
    while (p >= 0) {
      ans[r] <- arr[p]
      r <- r + 1
      p <- p - 2
    }
    p <- n - 2
    r <- (n+1)/2 - 1
    while (p >= 0) {
      ans[r] <- arr[p]
      r <- r - 1
      p <- p - 2
    }
    return(ans)
  }

  hydrograph <- rearr(inc_depth)
  plot(time_ser[-1]/60,hydrograph , ylab = "rainfall in mm",
       xlab = "time interval of 15 min each")
  lines(time_ser[-1]/60,hydrograph)
  barplot.default(hydrograph , ylab = "rainfall in mm",
                  xlab = "time interval of 15 min each")
  write.csv(data.frame(time_ser[-1]/60,hydrograph),"/Users/bedantsharma/Desktop/desktop/sem6/ce462/210260/Rcode/hydrograph.csv")
  
}




