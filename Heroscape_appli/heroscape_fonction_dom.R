

prob_att <- 3/6
prob_def <- 2/6

Dommage <- Vectorize(function(nb_att, de_att, de_def, vie_def){
  
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
            prob_dom_tot[pmin(i + j + k + z + 1, hit_max + 1)] <- prob_dom_tot[pmin(i + j + k + z + 1, hit_max + 1)] + prob_dom[i + 1] * prob_dom[j + 1] * prob_dom[k + 1] * prob_dom[z + 1]
          }
        }
      }
    }
  }
  
  return(prob_dom_tot)
})


prob_no_dam <- c()

for(att in 1:12){
  prob_no_dam <- c(prob_no_dam, Dommage(1, att, 1:12, 1)[1, ])
}

nb_col <- 12
nb_row <- 12

mat_prob <- matrix(prob_no_dam, nrow = nb_row, ncol = nb_col, byrow = TRUE)

nom_col <- c()
for(i in 1:nb_col){
  nom_col <- c(nom_col, paste0("Def", i))
}

nom_ligne <- c()
for(i in 1:nb_row){
  nom_ligne <- c(nom_ligne, paste0("Att", i))
}

colnames(mat_prob) <- nom_col
rownames(mat_prob) <- nom_ligne

saveRDS(mat_prob, file = "../Heroscape_appli/objets_save/mat_no_dam.rds")





