# Code for publication "Supplying whole blood with drones for prehospital transfusion at trauma sites in Finland - a simulation"

Simulations for delivery of whole blood to trauma sites with drones

plot_figures.Rmd plots all figures of the publication from precomputed data.

simulation_s100_d50_final.R is an example of carrying out the discrete event simulation.
Produces a list that contains simulation results of events (event locations) that is, service time, queue time, and BDC for the fastest option. To change drone parameters (speed and one-way range) change the parameters values 'speed' and 'Dis_limit' in the code
