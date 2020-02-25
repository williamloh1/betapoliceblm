VSG<-read_csv("~/Downloads/VOTER_Survey_Jan217_Release1-csv.csv")
VSG.inst <-VSG %>% 
  select(starts_with("inst"))
for(i in seq_along(VSG.inst)){
  VSG.inst[,i]= na_if(VSG.inst[[i]],VSG.inst[[i]]>4)
}
map(VSG.inst, mean, na.rm = TRUE)
map(VSG.inst, median, na.rm = TRUE)
