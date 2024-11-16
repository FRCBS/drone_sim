
# Alustukset
rm(list = ls())
start.time <- Sys.time()

# Lue traumaennusteet tiedostosta ja alusta simulointia varten
##############################################################
setwd("/users/erastopa/kokoveri/5")
ennusteet=read.csv(file = "Trauma_ennusteet_1x1.csv",header = TRUE,sep=";",dec=",")
ennusteet$skaalatut=ennusteet$trauma_saatto_enn/(365*24*60) # Muunnetaan tapahtuma-lukumääriksi minuutissa
ennusteet$skaalatut[ennusteet$skaalatut==0]=1.119e-09/(365*24*60)  # Ei sallita nollia, mutta laitetaan joku tosi pieni
intensities <- ennusteet$skaalatut

# Lue karttaruutujen etäisyystiedot (ovat sisällä matriisissa)
load('et_mat.RData')
# Ota vain 5 parasta
et_mat=et_mat[,c(4,11,16,23,24)]


# Simuloinnin parametrit
##################################################################
sim_time <- 365*24*60         # Simulaation aikakehys minuutteina
speed=100                     # Nopeus
Dis_limit=50                  # Maksimikantavuus yhteen suuntaan
N_drone=1                     # Droneja per base
N_base=ncol(et_mat)           # Beissien lkm
zero_vec=rep(0,N_base)        # Apuvektori nollia        
set.seed(123)                 # Set seed for reproducibility
N=1000                        # Simuloinnin koko


# Funktion määrittely tapahtuma-aikojen generointiin
##################################################################
generate_events <- function(intensities, sim_time) {
  num_points=length(intensities)
  events_list <- vector('list', num_points)
  
  # Luodaan tapahtumat jokaiselle karttaruudulle erikseen
  for (i in 1:num_points) {
    time <- 0
    events <- c()
    intensity <- intensities[i]
    
    while (time < sim_time) {
      # Generoidaan seuraavan tapahtuman aika eksponenttijakauman avulla
      interarrival_time <- rexp(1, rate = intensity)
      
      # Lisätään karttaruudun i tapahtuma listaan
      events <- c(events, time + interarrival_time)
      
      # Päivitetään aikaa seuraavaan tapahtumaan
      time <- time + interarrival_time
    }
    
    # Talletetaan listaan lokaatio ja sitä vastaavat tapahtumien ajanhetket
    events_list[[i]] <- list(events=events, location=i)
  }
  return(events_list)
}



#############################################################################
# Tästä alkaa makrotason koodi
##############################################################################


result_list <- vector('list', N)
for (k in 1:N){
  print(k)
  
  # Generoidaan tapahtumat aiemmin määritellyllä funktiolla
  events_list <- generate_events(intensities, sim_time)
  
  # Irrotetaan listasta event_list tapahtuma ajat ja lokaatiot matriisiin event_matrix
  event_times <- unlist(sapply(events_list, function(x) x$events))
  locations <- rep(sapply(events_list, function(x) x$location), sapply(events_list, function(x) length(x$events)))
  event_matrix <- rbind(event_times, locations)
  
  # Järjestä tapahtumat matriisissa kasvavaan aikajärjestykseen
  sorted_indices <- order(event_matrix[1, ]) # Get the indices that sort the event times
  sorted_event_times <- event_matrix[1, sorted_indices] # Sort event times
  sorted_locations <- event_matrix[2, sorted_indices] # Sort corresponding locations
  sorted_event_matrix_temp <- rbind(sorted_event_times, sorted_locations)
  
  # Ota pois ne ajanhetket, jotka menivät aikaikkunan ulkopuolelle
  sorted_event_matrix <- sorted_event_matrix_temp[,sorted_event_matrix_temp[1,]<=sim_time]
  
  # Määritellään kierroskohtaisia muuttujia
  N_ev=ncol(sorted_event_matrix)  # Tapahtumien lukumäärä tällä kierroksella
  End_times=rep(0,N_base)         # Jokaisen beissin edeltävän työn lopetusaika (minuutteina)
  wait_time_store=rep(-1,N_ev)    # Odotusaika jokaisen tapahtuman valmistumiseen 
  # ja jollei valmistu koskaan (liian kaukana) niin wait_time =-1
  queue_time_store=rep(-1,N_ev)   # Jonotusaika jokaisen tapahtuman valmistumiseen 
  # ja jollei valmistu koskaan (liian kaukana) niin wait_time =-1
  base_store=rep(-1,N_ev)         # Apumuuttuja, joka tallettaa tapahtuman hoitavan basen    
  
  # Käy läpi jokainen event-ajankohta
  for (i in 1:N_ev) {
    
    # Selvitetään tapahtuma-ajankohdan i lokaatioINDEKSI 
    location_index=sorted_event_matrix[2,i] 
    # location=ennusteet$nro[location_index]
    
    # Määritä valmistumisajat yhteen suuntaan kulkien jokaisesta beissistä
    travel_times=et_mat[location_index,]/speed*60                    # Matka-ajat=matka/nopeus
    queue_times=pmax(zero_vec,(End_times-sorted_event_matrix[1,i]))  # Jonotusajat edellisen valmistumisajankohdista 
    wait_times=travel_times+queue_times                              # Valmistumisajat = matka-ajat + jonotusajat
    
    # Haetaan ne baset, jotka alle kriittisen etäisyyden
    pot_base_ind <- which(et_mat[location_index,] < Dis_limit)  
    
    # Jos/kun tälläisiä baseja löytyy, niin...
    if (length(pot_base_ind)>0){
      
      # .. hae (=se base), joka olisi ensimmäisenä perillä 
      shortest_index=which.min(wait_times)
      #print(c(i, shortest_index))
      
      # Lasketaan matkaan kuluva aika nopeimmasta 
      travel_time=(et_mat[location_index, shortest_index]/speed)*60
      travel_time=travel_times[shortest_index]
      
      queue_time=queue_times[shortest_index]

      # Määritellään valitun basen seuraava valmistumisaika
      # Tässä matka-aika on kahteen kertaan (edestakaisin)
      End_times[shortest_index]=sorted_event_matrix[1,i]+queue_time+2*travel_time  
      
      wait_time_store[i]=queue_time+travel_time # Kokonaisodotusaika talteen
      queue_time_store[i]=queue_time            # Jonotusaika talteen
      base_store[i]=shortest_index              # Base talteen
    } # jos löytyy baseja alle kriittisen etäisyyden
  } # for-luuppi yli tapahtumien
  
  # Talletukset tälle kierrokselle
  result_list[[k]] <- list(wait_times=wait_time_store, queue_times=queue_time_store, locations=sorted_event_matrix[2,],bases=base_store )
}


end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken


save(result_list, file = "result_s100_d50.RData")






