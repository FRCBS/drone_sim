
# Initializations
rm(list = ls())
start.time <- Sys.time()

# Read/define the trauma intensities (per minute) 
# into a vector variable 'intensities'
##############################################################
intensities=rep(1e-5,100000) # This is just an example


# Read/define the distances for each map grid (row) to each BDC (column) 
# into a matrix variable 'dist_mat'
###############################################################################
dist_mat=matrix(rep(1:100000, 25), ncol = 25)


# Set the simulation parameters
##################################################################
sim_time <- 365*24*60         # Simulation time frame
speed=100                     # Drone speed in km/h
Dis_limit=50                  # Max range to _one_ direction (the overall range is double)
N_drone=1                     # Drones per BDC
N_base=ncol(dist_mat)         # Number of BDC's
N=10                          # Size of simulations 

zero_vec=rep(0,N_base)        # Vector of zeros         
set.seed(123)                 # Set seed for reproducibility



# Define function to generate event times
##################################################################
generate_events <- function(intensities, sim_time) {
  num_points=length(intensities)
  events_list <- vector('list', num_points)
  
  # Create events to each map grid separately
  for (i in 1:num_points) {
    time <- 0
    events <- c()
    intensity <- intensities[i]
    
    while (time < sim_time) {
      # Generate the next event time 
      interarrival_time <- rexp(1, rate = intensity)
      
      # Add the event to the list of event times of map grid i
      events <- c(events, time + interarrival_time)
      
      # Update the time variable
      time <- time + interarrival_time
    }
    
    # Store the events of location i
    events_list[[i]] <- list(events=events, location=i)
  }
  return(events_list)
}



#############################################################################
# The macro level code starts here
##############################################################################


result_list <- vector('list', N)
for (k in 1:N){
  
  print(k) 
  
  # Generate the events using the function defined above
  events_list <- generate_events(intensities, sim_time)
  
  # Distill and reframe the events and location into matrix 'event_matrix'
  event_times <- unlist(sapply(events_list, function(x) x$events))
  locations <- rep(sapply(events_list, function(x) x$location), sapply(events_list, function(x) length(x$events)))
  event_matrix <- rbind(event_times, locations)
  
  # Sort the generated events into increasing order by time 
  sorted_indices <- order(event_matrix[1, ])            # Get the indices that sort the event times
  sorted_event_times <- event_matrix[1, sorted_indices] # Sort event times
  sorted_locations <- event_matrix[2, sorted_indices]   # Sort corresponding locations
  sorted_event_matrix_temp <- rbind(sorted_event_times, sorted_locations)
  
  # Take out the events that are outside of the simulation time frame
  sorted_event_matrix <- sorted_event_matrix_temp[,sorted_event_matrix_temp[1,]<=sim_time]
  
  # Define some additional variables 
  N_ev=ncol(sorted_event_matrix)  # Number of events within this simulation round
  End_times=rep(0,N_base)         # Initialize the end times 
  
  # Initialize saved variables for this simulation round 
  wait_time_store=rep(-1,N_ev)    # Variable describing the _wait time_ of each event
                                  # ... which gets value -1 in case the event cannot be handled) 
  queue_time_store=rep(-1,N_ev)   # Variable describing the _queue time_ of each event 
                                  # ... which gets value -1 in case the event cannot be handled) 
  base_store=rep(-1,N_ev)         # Variable storing the base (BCD) of each event     
                                  # ... which gets value -1 in case the event cannot be handled) 
  
  # Go through the events for this simulation round
  for (i in 1:N_ev) {
    
    # Check the location index of the event i 
    location_index=sorted_event_matrix[2,i] 
  
    # Calculate different time variables for this event 
    travel_times=dist_mat[location_index,]/speed*60                    # Travel (flight) times in minutes
    queue_times=pmax(zero_vec,(End_times-sorted_event_matrix[1,i]))  # Potential queue times (before the previous is finished)  
    wait_times=travel_times+queue_times                              # Waiting time= travel time + queue time
    
    # Find the bases (BDC's) that are within the oneway range
    pot_base_ind <- which(dist_mat[location_index,] < Dis_limit)  
    
    # And if there are such..
    if (length(pot_base_ind)>0){
      
      # .. then select the base that has the fastest delivery time 
      shortest_index=which.min(wait_times)
      travel_time=travel_times[shortest_index]
      queue_time=queue_times[shortest_index]

      # Calculate the finish time of the event (drone is back again)
      # as the event time + possible queue time + flight time (return)
      End_times[shortest_index]=sorted_event_matrix[1,i]+queue_time+2*travel_time  
      
      wait_time_store[i]=queue_time+travel_time # Store the wait time (queue + flight)
      queue_time_store[i]=queue_time            # Store the queue time
      base_store[i]=shortest_index              # Store the base
    } #  if (length(pot_base_ind)>0)
  } #  for (i in 1:N_ev)
  

  
  # Store the variables for this simulation round into a list:
  # -wait_times: waiting times describing the time how long a patient need to wait before service
  # -queue_times: how long does it take to wait until the drone is available from the previous service
  # -locations: location indices of the events
  # -bases: base (BDC) indices that provides the fastest service
  # NOTE: value is -1 is stored if the drone is out of reach (except for locations)
  
  result_list[[k]] <- list(wait_times=wait_time_store, queue_times=queue_time_store, locations=sorted_event_matrix[2,], bases=base_store)
}









