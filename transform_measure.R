# turn alpha estimate into partisan measure

transform_measure <- function(alpha,J){
  try <- data.frame(u = rep(1,J),
                    alpha = alpha)
  try$delta <- try$u + try$alpha
  try$prob_0 <- exp(try$u) / sum(exp(try$u))
  try$prob_1 <- exp(try$delta) / sum(exp(try$delta))
  try$rho <- try$prob_1 / (try$prob_0 + try$prob_1)
  try$partisanship <- 0.5*(try$prob_1*try$rho + try$prob_0*(1-try$rho))
  return(sum(try$partisanship))  
}