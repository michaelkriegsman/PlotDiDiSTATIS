#Plot_DiMDS_Perm_Rows_r2disc_perm.b_perm.R
#'Plot results from test of Null Hypothesis on Rows
#'
#'
#'@param res_DiMDS Output from DiMDS_Perm_Rows
#'@param Title Name the figure
#'@return Histogram of the r2 under the null
#'@export


Plot_DiMDS_Perm_Rows_r2disc_perm.b_perm <- function(res_DiMDS, Title = "r2disc_perm.b_perm"){

  # dev.new()
  hist(res_DiMDS$Perm_Rows$r2disc_perm.b_perm, breaks=50, main=Title)
  abline(v = quantile(res_DiMDS$Perm_Rows$r2disc_perm.b_perm, .95), col="red", lwd=2, lty=2)
  abline(v = res_DiMDS$res_Disc_Full$proj2Bary$r2disc.B, col='blue', lwd = 3)

}
