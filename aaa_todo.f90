!! TODO LIST TO ADD POLLUTANTS TO SWAT PROCCES BASED ON PESTICIDE IMPLEMENTATION AND EXCO_OM
    
    !! EXCO_OM
    
!! - Object to type data is hyd_output in hydrograph_module.f90
!! - The name of variable to for export coeficient (exco) is exco 
!!   import from hydrograph_module.f90 and filled in exco_read_om.f90

!! - Objecte clau type object_connectivity dins hydrograph_module.f90 definit com a ob
    
    !! PESTICIDES
    
!! - type (cs_water_init_concentrations), dimension(:),allocatable:: pest_water_ini definit a constituen_mass_module.f90
!! - a la linia 226 de sd_hydsed_init.f90 hi ha on es defineixen els valors de pesticides pels canals
!! - Fitxer on es fa la propagacio de pesticides ch_rtpest.f90