gather_drug_data = function(data) {
  drug_data = subset(data, select = c(2:20, 27))
  
  cannabis_data = gather(drug_data[c(2:4,6:7,20)], "model", "response", 4:5, na.rm=T)
  cannabis_data$drug = 'cannabis'
  cannabis_data$model[cannabis_data$model == 'cannabis_markov'] <- 'markov'
  cannabis_data$model[cannabis_data$model == 'cannabis_mistral'] <- 'mistral'
  
  mdma_data = gather(drug_data[c(2:4,9:10,20)], "model", "response", 4:5, na.rm=T)
  mdma_data$drug = 'mdma'
  mdma_data$model[mdma_data$model == 'mdma_markov'] <- 'markov'
  mdma_data$model[mdma_data$model == 'mdma_mistral'] <- 'mistral'
  
  lsd_data = gather(drug_data[c(2:4,12:13,20)], "model", "response", 4:5, na.rm=T)
  lsd_data$drug = 'lsd'
  lsd_data$model[lsd_data$model == 'lsd_markov'] <- 'markov'
  lsd_data$model[lsd_data$model == 'lsd_mistral'] <- 'mistral'
  
  salvia_data = gather(drug_data[c(2:4,15:16,20)], "model", "response", 4:5, na.rm=T)
  salvia_data$drug = 'salvia'
  salvia_data$model[salvia_data$model == 'salvia_markov'] <- 'markov'
  salvia_data$model[salvia_data$model == 'salvia_mistral'] <- 'mistral'
  
  mushroom_data = gather(drug_data[c(2:4,18:19,20)], "model", "response", 4:5, na.rm=T)
  mushroom_data$drug = 'mushroom'
  mushroom_data$model[mushroom_data$model == 'mushroom_markov'] <- 'markov'
  mushroom_data$model[mushroom_data$model == 'mushroom_mistral'] <- 'mistral'
  
  drug_data = rbind(cannabis_data, mdma_data, lsd_data, salvia_data, mushroom_data)
  drug_data$model = as.factor(drug_data$model)
  drug_data$drug = as.factor(drug_data$drug)
  
  return(drug_data)
}
