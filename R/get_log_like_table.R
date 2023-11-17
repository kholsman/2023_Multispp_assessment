#'
#'
#'
#' create assessment table output
#' 

# log_input(offset_fsh)
# log_input(srv_biom_lse)
# log_input(offset_srv)
# log_input(offset_eit)

get_loglik_sub <- function(modIN, obj_nm = c( "M1","M2",     
                                       "srv_sel", "fsh_sel",
                                       "srv_bio", "fsh_cat",
                                       "srv_age", "fsh_age"), 
                    obj_nm2 = c( "fsh_sel_pen", "srv_sel_pen",
                                 "rec_dev_prior", "init_dev_prior",
                                 "F_dev_prior")){
  
  i <- 0
  for(sp in 1:3){
    for(nm in obj_nm){
      i <- i + 1
      nmtmp <- as.numeric(eval(parse(text= paste0("modIN$",nm,"_like_",sp))))
      tmp <- data.frame(name = nm, species= sp, value = nmtmp )
      if(i ==1){
        ll_table <- tmp
      }else{
        ll_table <- rbind(ll_table, tmp)
      }
    }
  }
  
  for(sp in 1:3){
    for(nm in obj_nm2){
      i <- i + 1
      nmtmp <- as.numeric(eval(parse(text= paste0("modIN$",nm,"_",sp))))
      tmp <- data.frame(name = nm, species= sp, value = nmtmp )
      if(i ==1){
        ll_table <- tmp
      }else{
        ll_table <- rbind(ll_table, tmp)
      }
    }
  }
  
  
  ll_table <- rbind(ll_table, data.frame(name = "eit_srv", species =1, value = modIN$eit_srv_like))
  ll_table <- rbind(ll_table, data.frame(name = "eit_age", species =1, value = modIN$eit_age_like))
  return(ll_table)
}
ll_table_2 <- get_loglik_sub(modIN = ceattle_2)
ll_table_0 <- get_loglik_sub(modIN = ceattle_0)

sum(ll_table_0$value)- ceattle_0$obj_fun
sum(ll_table_2$value)- ceattle_2$obj_fun



ll_table_0%>%mutate(value=round(value,2))%>%cast(name~species)
ll_table_2%>%mutate(value=round(value,2))%>%cast(name~species)

ll_table_0%>%mutate(value=round(100*value/as.numeric(ceattle_0$obj_fun),2))%>%cast(name~species)
ll_table_2%>%mutate(value=round(100*value/as.numeric(ceattle_2$obj_fun),2))%>%cast(name~species)

get_propBinobs <- function(modIN = ceattle_2){
  i <- 0
  for(sp in 1:3){
    i <- i +1
     eval(parse(text =  paste0("
      tmpB <- data.frame(
        surv_b = as.numeric(modIN$srv_bio_",sp,"),
        surv_b_se = as.numeric(modIN$srv_bio_se_",sp,"), 
        surv_b_hat = as.numeric(modIN$srv_bio_hat_",sp,"))")))
      tmpB <- tmpB%>%mutate(lower=  surv_b-surv_b_se, upper =  surv_b+surv_b_se)%>%rowwise()%>%mutate(within = surv_b_hat>=lower & surv_b_hat<=upper )
      tmpB$within_n <- 0
      tmpB$within_n[which(tmpB$within)]<-1
      
      tmpB$species <- sp
      if(i ==1){
        srvB_2 <- tmpB
      }else{
        srvB_2 <- rbind(srvB_2, tmpB)
      }
  }
  return(data.frame(srvB_2))
}
srvB_2 <- get_propBinobs(modIN = ceattle_2)
srvB_0 <- get_propBinobs(modIN = ceattle_0)

data.frame(srvB_2%>%group_by(species)%>%summarize(sumin = sum(within_n), n = length(within_n), prop_prcnt  = round(100*sumin/n) ))
data.frame(srvB_0%>%group_by(species)%>%summarize(sumin = sum(within_n), n = length(within_n), prop_prcnt  = round(100*sumin/n) ))

# Ceattle_2
# # curv_pen_fsh
# 12.5 12.5 12.5
# # curv_pen_srv
# 12.5 12.5 12.5
# # tau
# 200
# # offset_fsh
# 15748.9 21756.9 21147.1
# # srv_biom_lse
# 0.17057 0.0967551 0.124772 0.0951159 0.116389 0.151764 0.13187 0.111749 0.177837 0.17703 0.153483 0.109446 0.14278 0.331423 0.111641 0.175667 0.14894 0.154416 0.15507 0.0902976 0.1153 0.390506 0.107403 0.110136 0.108115 0.112845 0.123666 0.148838 0.148471 0.099787 0.113254 0.0858359 0.0719808 0.0638446 0.0998331 0.0687045 0.185161 0.11219 0.138002 0.138002 0.139321
# 0.0794792 0.118975 0.087152 0.133718 0.105898 0.0690955 0.103113 0.132151 0.10242 0.0884775 0.096647 0.0875356 0.12936 0.117777 0.180148 0.142834 0.0683118 0.0894985 0.0792713 0.0657097 0.0779604 0.0963811 0.0808078 0.139161 0.0588663 0.156264 0.0756587 0.045434 0.127599 0.0830705 0.068066 0.144905 0.0962108 0.103601 0.0840039 0.0912053 0.057207 0.0806043 0.0831975 0.0831975 0.0798724
# 0.108566 0.0922898 0.142979 0.0898226 0.101791 0.0748495 0.117502 0.104593 0.094688 0.13105 0.142946 0.104065 0.108899 0.144043 0.106381 0.128341 0.113952 0.256641 0.168993 0.0937669 0.102324 0.0913837 0.073589 0.0766672 0.0793726 0.0808304 0.0880814 0.10258 0.0845542 0.078051 0.107693 0.088662 0.0691909 0.0615383 0.0484457 0.0653906 0.0692255 0.174716 0.0896448 0.0896448 0.0898185
# # offset_srv
# 8502.92 11489.5 11752.9
# # offset_eit

# # curv_pen_fsh
# 12.5 12.5 12.5
# # curv_pen_srv
# 12.5 12.5 12.5
# # tau
# 200
# # offset_fsh
# 15748.9 21756.9 21147.1
# # srv_biom_lse
# 0.17057 0.0967551 0.124772 0.0951159 0.116389 0.151764 0.13187 0.111749 0.177837 0.17703 0.153483 0.109446 0.14278 0.331423 0.111641 0.175667 0.14894 0.154416 0.15507 0.0902976 0.1153 0.390506 0.107403 0.110136 0.108115 0.112845 0.123666 0.148838 0.148471 0.099787 0.113254 0.0858359 0.0719808 0.0638446 0.0998331 0.0687045 0.185161 0.11219 0.138002 0.138002 0.139321
# 0.0794792 0.118975 0.087152 0.133718 0.105898 0.0690955 0.103113 0.132151 0.10242 0.0884775 0.096647 0.0875356 0.12936 0.117777 0.180148 0.142834 0.0683118 0.0894985 0.0792713 0.0657097 0.0779604 0.0963811 0.0808078 0.139161 0.0588663 0.156264 0.0756587 0.045434 0.127599 0.0830705 0.068066 0.144905 0.0962108 0.103601 0.0840039 0.0912053 0.057207 0.0806043 0.0831975 0.0831975 0.0798724
# 0.108566 0.0922898 0.142979 0.0898226 0.101791 0.0748495 0.117502 0.104593 0.094688 0.13105 0.142946 0.104065 0.108899 0.144043 0.106381 0.128341 0.113952 0.256641 0.168993 0.0937669 0.102324 0.0913837 0.073589 0.0766672 0.0793726 0.0808304 0.0880814 0.10258 0.0845542 0.078051 0.107693 0.088662 0.0691909 0.0615383 0.0484457 0.0653906 0.0692255 0.174716 0.0896448 0.0896448 0.0898185
# # offset_srv
# 8502.92 11489.5 11752.9
# # offset_eit
# 1434.31

# 1434.31