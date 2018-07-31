# read data from source
df = read.csv2(file = '/media/hadimaster/Data 2/Kerjaan/Kerjaan Statistik/Time Series/data saja.csv')
# change 'data' datatype to numeric
df['data'] = as.numeric(df[, 4])
# take 'no' and 'data' column
df.time_series = data.frame(df[, 2], df[, 4])
# change column name
colnames(df.time_series) = c("no", "data")
# see df.time_series
df.time_series

# find the period 
c = 12

# set a and d with value from 0 and 1
a1 = 0
a2 = 0
a3 = 0
d1 = 1
d2 = 1
d3 = 1

# golden section
goldenRatio = 0.618
# Set iteration and error tolerance
iterasi = 1000
toleransi = 0.00001

# Set alpha1 and alpha2
alpha1 = goldenRatio * a1 + (1 - goldenRatio) * d1
print("alpha1 value")
print(alpha1)
alpha2 = a1 + d1 - alpha1
print("alpha2 value")
print(alpha2)
# Set beta1 and beta2
beta1 = goldenRatio * a2 + (1 - goldenRatio) * d2
print("beta1 value")
print(beta1)
beta2 = a2 + d2 - beta1
print("beta2 value")
print(beta2)
# Set gama1 and gama2
gama1 = goldenRatio * a3 + (1 - goldenRatio) * d3
print("gama1 value")
print(gama1)
gama2 = a3 + d3 - gama1
print("gama2 value")
print(gama2)

# Generate all possible cases to optimize our algorithm
# There is 8 possibilities, that is
# alpha1, beta1, gama1
# alpha1, beta1, gama2
# alpha1, beta2, gama1
# alpha1, beta2, gama2
# alpha2, beta1, gama1
# alpha2, beta1, gama2
# alpha2, beta2, gama1
# alpha2, beta2, gama2
possible = expand.grid(c("alpha1", "alpha2"), c("beta1", "beta2"), c("gama1", "gama2"))
possible = possible[order(possible$Var1, possible$Var2),]
# Seeing all possible cases
possible
# Convert all to character 
possible$Var1 =  as.character(possible$Var1)
possible$Var2 =  as.character(possible$Var2)
possible$Var3 =  as.character(possible$Var3)

# Set name for all cases
fx_list = c()
for (i in 1:nrow(possible)) {
  fx_list[i] = paste(possible[i,], collapse = ",")
}

# insert to possible dataframe as label
possible$label = fx_list

# Initiate vector to save all MAPE
df_mape = c()

# Initiate dataframe to save all result
df_hasil = data.frame(
  a1 = 0,
  d1 = 0,
  a2 = 0,
  d2 = 0,
  a3 = 0,
  d3 = 0,
  f_alpha1_beta1_gama1 = 0,
  f_alpha1_beta1_gama2 = 0,
  f_alpha1_beta2_gama1 = 0,
  f_alpha1_beta2_gama2 = 0,
  f_alpha2_beta1_gama1 = 0,
  f_alpha2_beta1_gama2 = 0,
  f_alpha2_gama2_beta1 = 0,
  f_alpha2_beta2_gama2 = 0
)

# Initiate dataframe to save  alpha, beta dan gama value
# for every iteration
df_alpha_beta_gama = data.frame(
  alpha1 = 0,
  alpha2 = 0,
  beta1 = 0,
  beta2 = 0,
  gama1 = 0,
  gama2 = 0
)


for (iter in 1:iterasi) {
  # Save alpha, beta and gama into df_alpha_beta_gama
  df_alpha_beta_gama[iter,] = c(alpha1, alpha2, beta1, beta2, gama1, gama2)
  print(paste0("iterasi ke-", iter))
  # Take data to analyze for time series analysis
  data = df.time_series[, 2]
  for (urutan in 1:nrow(possible)) {
    # Initiate Lt, Bt, St, Ft, Et, MSE vector
    Lt = c()
    Bt = c()
    St = c()
    Ft = c()
    Et = c()
    MSE = c()
    # Insert alpha, beta and gama from 'possible' dataframe
    alpha = get(possible[urutan, 1])
    beta = get(possible[urutan, 2])
    gama = get(possible[urutan, 3])
    for (i in 1:(length(data) - 11)) {
      # for i = 1
      if (i == 1) {
        # Find Lt when t = 1
        Lt[i] = mean(data[1:c])
        # Find Bt when t = 1
        Bt[i] = sum(data[13:(13 + c - 1)] - data[1:12]) / (c ^ 2)
        # Generate St from month 1 - 12
        for (j in 1:c) {
          St[j] = data[j] - Lt[i]
        }
        j = c + i
      } else {
        # i = i+1
        # for i >1
        # Find Lt for i > 1
        Lt[i] = alpha*(data[j])-St[i-1]+(1 - alpha)*(Lt[i-1] + Bt[i-1])
        # Find Bt for i > 1
        Bt[i] = beta*(Lt[i] - Lt[i - 1]) + (1 - beta) * Bt[i - 1]
        # Find St for i > 1
        St[j] = gama * (data[j] - Lt[i]) + (1 - gama) * St[i - 1]
        # Find Ft
        Ft[i - 1] = Lt[i - 1] + Bt[i - 1] + St[i - 1]
        # Find Et
        Et[i - 1] = data[j] - Ft[i - 1]
        # Find MSE
        MSE[i - 1] = Et[i - 1] * Et[i - 1]
        j = c + i
      }
    }
    # Initiate vector to calculae MAPE
    absSum = c()
    jumlah = data[13:length(data)]
    for (i in 1:length(jumlah)) {
      absSum[i] = abs(jumlah[i]-Ft[i])/abs(jumlah[i])
    }
    # Calculate MAPE
    nilaiMAPE = (1/length(jumlah))*sum(absSum)
    # Show MAPE
    print("MAPE")
    print(nilaiMAPE)
    # Save all MAPE into df_mape
    df_mape[urutan] = nilaiMAPE
  }
  # Generate a dataframe to save all result into result_df
  result_df = data.frame(mape = df_mape, label = fx_list)
  # insert a1, a2, a3, d1, d2 and d3 and MAPE into df_hasil
  df_hasil[iter,] = c(a1, d1, a2, d2, a3, d3, df_mape)
  # Find MAPE based on all possible cases
  # and insert it into bind_colm
  bind_colm = cbind.data.frame(possible, df_mape)
  # Find the difference between parameter
  print("result")
  print("difference")
  print("d1 - a1")
  print(d1 - a1)
  print("d2 - a2")
  print(d2 - a2)
  print("d3 - a3")
  print(d3 - a3)
  # Check if all difference above is small than our error/tolerance value
  print("Is d1 - a1 < tolerance ?")
  print(d1 - a1 <= toleransi)
  print("is d2 - a2 < tolerance ?")
  print(d2 - a2 <= toleransi)
  print("Is d3 - a3 < tolerance ?")
  print(d3 - a3 <= toleransi)
  Sys.sleep(2)
  if ((d1 - a1 <= toleransi) || (d2 - a2 <= toleransi) || (d3 - a3 <= toleransi)) {
    print("Iteration finished")
    # Do again all the process above with optimized
    # alpha, beta and gama based on result above
    Lt = c()
    Bt = c()
    St = c()
    Ft = c()
    Et = c()
    MSE = c()
    data = df.time_series[, 2]
    # Take alpha, beta and gama from bind_colm
    # where it has smallest MAPE
    alpha = get(bind_colm[bind_colm$df_mape == min(bind_colm$df_mape), ][, 1])
    beta = get(bind_colm[bind_colm$df_mape == min(bind_colm$df_mape), ][, 2])
    gama = get(bind_colm[bind_colm$df_mape == min(bind_colm$df_mape), ][, 3])
    for (i in 1:(length(data) - 11)) {
      if (i == 1) {
        Lt[i] = mean(data[1:c])
        Bt[i] = sum(data[13:(13 + c - 1)] - data[1:12]) / (c ^ 2)
        for (j in 1:c) {
          St[j] = data[j] - Lt[i]
        }
        j = c + i
      } else {
        Lt[i] = alpha * (data[j]) - St[i - 1] + (1 - alpha) * (Lt[i -
                                                                    1] + Bt[i - 1])
        Bt[i] = beta * (Lt[i] - Lt[i - 1]) + (1 - beta) * Bt[i - 1]
        St[j] = gama * (data[j] - Lt[i]) + (1 - gama) * St[i - 1]
        Ft[i - 1] = Lt[i - 1] + Bt[i - 1] + St[i - 1]
        Et[i - 1] = data[j] - Ft[i - 1]
        MSE[i - 1] = Et[i - 1] * Et[i - 1]
        j = c + i
      }
    }
    absSum = c()
    jumlah = data[13:length(data)]
    for (i in 1:length(jumlah)) {
      absSum[i] = abs(jumlah[i]-Ft[i])/abs(jumlah[i])
    }
    nilaiMAPE = (100/length(Ft))*sum(absSum)
    print("nilai MAPE")
    print(nilaiMAPE)
    break
  } else {
    # If we not get optimized MAPE, change a1, a2, a3, d1, d2, d3 and alpha, beta and gama
    # based on golden-section rule
    if (result_df[result_df$mape == max(result_df$mape), ][, 2] == fx_list[1]) {
      print("f(alpha1, beta1, gama1) max")
      a1 = alpha1
      alpha1 = alpha2
      alpha2 = a1 + d1 - alpha1
      
      a2 = beta1
      beta1 = beta2
      beta2 = a2 + d2 - beta1
     
      a3 = gama1
      gama1 = gama2
      gama2 = a3 + d3 - gama1
    } else if (result_df[result_df$mape == max(result_df$mape), ][, 2] == fx_list[2]) {
      print("f(alpha1, beta1, gama2) max")
      a1 = alpha1
      alpha1 = alpha2
      alpha2 = a1 + d1 - alpha1
      
      a2 = beta1
      beta1 = beta2
      beta2 = a2 + d2 - beta1
      
      d3 = gama2
      gama2 = gama1
      gama1 = goldenRatio * a3 + (1 - goldenRatio) * d3
    } else if (result_df[result_df$mape == max(result_df$mape), ][, 2] == fx_list[3]) {
      print("f(alpha1, beta2, gama1) max")
      a1 = alpha1
      alpha1 = alpha2
      alpha2 = a1 + d1 - alpha1
     
      d2 = beta2
      beta2 = beta1
      beta1 = goldenRatio * a2 + (1 - goldenRatio) * d2
      
      a3 = gama1
      gama1 = gama2
      gama2 = a3 + d3 - gama1
    } else if (result_df[result_df$mape == max(result_df$mape), ][, 2] == fx_list[4]) {
      print("f(alpha1, beta2, gama2) max")
      a1 = alpha1
      alpha1 = alpha2
      alpha2 = a1 + d1 - alpha1
      
      d2 = beta2
      beta2 = beta1
      beta1 = goldenRatio * a2 + (1 - goldenRatio) * d2
      
      d3 = gama2
      gama2 = gama1
      gama1 = goldenRatio * a3 + (1 - goldenRatio) * d3
    } else if (result_df[result_df$mape == max(result_df$mape), ][, 2] == fx_list[5]) {
      print("f(alpha2, beta1, gama1) max")
      d1 = alpha2
      alpha2 = alpha1
      alpha1 = goldenRatio * a1 + (1 - goldenRatio) * d1
      
      a2 = beta1
      beta1 = beta2
      beta2 = a2 + d2 - beta1
      
      a3 = gama1
      gama1 = gama2
      gama2 = a3 + d3 - gama1
    } else if (result_df[result_df$mape == max(result_df$mape), ][, 2] == fx_list[6]) {
      print("f(alpha2, beta1, gama2) max")
      d1 = alpha2
      alpha2 = alpha1
      alpha1 = goldenRatio * a1 + (1 - goldenRatio) * d1
      
      a2 = beta1
      beta1 = beta2
      beta2 = a2 + d2 - beta1
      
      d3 = gama2
      gama2 = gama1
      gama1 = goldenRatio * a3 + (1 - goldenRatio) * d3
    } else if (result_df[result_df$mape == max(result_df$mape), ][, 2] == fx_list[7]) {
      print("f(alpha2, beta2, gama1) max")
      d1 = alpha2
      alpha2 = alpha1
      alpha1 = goldenRatio * a1 + (1 - goldenRatio) * d1
      
      d2 = beta2
      beta2 = beta1
      beta1 = goldenRatio * a2 + (1 - goldenRatio) * d2
      
      a3 = gama1
      gama1 = gama2
      gama2 = a3 + d3 - gama1
    } else if (result_df[result_df$mape == max(result_df$mape), ][, 2] == fx_list[8]) {
      print("f(alpha2, beta2, gama2) max")
      d1 = alpha2
      alpha2 = alpha1
      alpha1 = goldenRatio * a1 + (1 - goldenRatio) * d1
      
      d2 = beta2
      beta2 = beta1
      beta1 = goldenRatio * a2 + (1 - goldenRatio) * d2

      d3 = gama2
      gama2 = gama1
      gama1 = goldenRatio * a3 + (1 - goldenRatio) * d3
    }
  }
}

# Show the result
print(df_hasil)
print(df_alpha_beta_gama)

# initiate vector for forecasting
ramalan = c()

# Inititate holt-winter forecasting
for (i in 1:length(data)) {
  if (i <= 12) {
    ramalan[i] = NA
  } else {
    ramalan[i] = Ft[i - 12]
  }
  
}

# Set how far we want to forecast
periodeRamalan = 12

# Find seasonal value for forecast
seasonalPeramalan = St[(length(St) - periodeRamalan + 1):length(St)]

# Initiate another vector for forecast
ramalan2 = c()
k = 0

# Do forecasting
for (i in 1:(length(data) + periodeRamalan)) {
  if (i <= length(data)) {
    ramalan2[i] = NA
  } else {
    k = k + 1
    ramalan2[i] = Lt[length(Lt)] + (Bt[length(Bt)] * k) + seasonalPeramalan[k]
  }
}

# See the forecast
ramalan2[!is.na(ramalan2)]

# plot data
plot(
  y = data,
  x = c(1:length(data)),
  xlab = 'periode',
  ylab = 'data',
  type = 'o',
  col = 'red',
  xlim = range(0:(length(ramalan2) + 10)),
  ylim = range(0:(max(ramalan2[!is.na(ramalan2)]) + 10000))
)
# Draw forecasted values of holt-winter additive (past to present)
lines(x = ramalan, col = 'blue')
# Draw forecasted values of holt-winter additive (future)
lines(x = ramalan2, col = 'green', type = 'o')
