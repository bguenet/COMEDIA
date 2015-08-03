# COMEDIA

Models of C cycle in aquatic ecosystem develloped by B. Guenet (bertrand.guenet@lsce.ipsl.fr) for scientific use only. 

The models aims at representing a simple but complete C cycle in a simplified aquatic ecosystems (without predators or herbivores, viral lysis, etc.).
The orginilaity of the models is to take represent implicetly the priming effect based on the equations of Guenet et al., (2013) in GMD (model G) or 
Wutlzer and Reichstein (2008) in BG (model W) or only a classical first order kinetic (F).
Another alternative is to represent the POC decomposition based on 1st order kinetics only (model F)/
The model G is simplest with less pools (5) and less parameters (8) comparted to model W (6 pools and 9 parameters)

The model represents explicitely a 13C tracer.
The time step of the model is daily.
To calculate the Henry constant we used the eq. 18 of Sander ACP 2014
The phytoplankton growth rate is based on the PROTECH model (Elliot et al 2001, Freshwater biology) where we applied a 
scalar based on Schippers et al., 2004 (Ecosystems) to take into account the limitation by the DIC. 
