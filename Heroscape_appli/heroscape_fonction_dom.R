

prob_att <- 3/6
prob_def <- 2/6

Dommage <- function(nb_att, de_att, de_def, vie_def){
  
  hit_max <- vie_def
  prob_dom_tot <- numeric(hit_max + 1)
  prob_dom <- numeric(hit_max + 1)
  
  for(def in 0:de_def){
    for(att in 0:de_att){
      hit <- pmax(pmin(att - def, hit_max), 0)
      prob_dom[hit + 1] <- prob_dom[hit + 1] + dbinom(att, de_att, prob_att) * dbinom(def, de_def, prob_def)
    }
  }

  if(nb_att == 1){
    for(i in 1:length(prob_dom)){
      prob_dom_tot[i] <- prob_dom[i]
    }
  }
  
  if(nb_att == 2){
    for(i in 0:hit_max){
      for(j in 0:hit_max){
        prob_dom_tot[pmin(i + j + 1, hit_max + 1)] <- prob_dom_tot[pmin(i + j + 1, hit_max + 1)] + prob_dom[i + 1] * prob_dom[j + 1]
      }
    }
  }
  
  if(nb_att == 3){
    for(i in 0:hit_max){
      for(j in 0:hit_max){
        for(k in 0:hit_max){
          prob_dom_tot[pmin(i + j + k + 1, hit_max + 1)] <- prob_dom_tot[pmin(i + j + k + 1, hit_max + 1)] + prob_dom[i + 1] * prob_dom[j + 1] * prob_dom[k + 1]
        }
      }
    }
  }
  
  if(nb_att == 4){
    for(i in 0:hit_max){
      for(j in 0:hit_max){
        for(k in 0:hit_max){
          for(z in 0:hit_max){
            prob_dom_tot[pmin(i + j + k + z + 1, hit_max + 1)] <- prob_dom_tot[pmin(i + j + k + z + 1, hit_max + 1)] 
                                                                  + prob_dom[i + 1] * prob_dom[j + 1] * prob_dom[k + 1] * prob_dom[z + 1]
          }
        }
      }
    }
  }
  
  return(prob_dom_tot)
}








