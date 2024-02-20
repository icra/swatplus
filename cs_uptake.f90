      subroutine cs_uptake !rtb cs
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine simulates constituent uptake in the root zone

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      
      use basin_module
      use organic_mineral_mass_module
      use hru_module, only : ep_day, ihru 
      use hydrograph_module
      use output_landscape_module
      use cs_module
      use constituent_mass_module
      use plant_data_module
      use plant_module
      use soil_module
      
      implicit none

      integer :: j                  !none        |HRU ID
      integer :: idp                !none        |plant database ID
      integer :: jj                 !none        |soil layer counter
      integer :: ics                !            |constituent ion counter
      real    :: depth              !none        |depth of soil layer, from ground surface
      real    :: rd                 !mm          |current rooting depth of plant
      real    :: rm                 !kg          |current root mass of plant
      real    :: rm_layer           !kg          |root mass in the soil layer
      real    :: rm_fract(50)       !            |fraction of root mass in the soil layer
      real    :: irrig_mass         !kg          |total constituent mass in irrigation water
      real    :: uptake_mass_total  !kg          |total uptake mass in soil layer
      real    :: uptake_mass        !kg/ha       |uptake mass in soil layer per unit area
      integer :: dum
      
      
      
      !HRU id
      j = ihru
      
      !plant ID
      idp = pcom(j)%plcur(1)%idplt

      !only proceed if there is rooting depth and root mass
      if(pcom(j)%plg(1)%root_dep > 0. .and. pl_mass(j)%root(1)%m > 0.) then
        
        !determine the fraction of root mass that is in each layer
        rd = pcom(j)%plg(1)%root_dep !root depth (mm) 
        rm = pl_mass(j)%root(1)%m * ob(j)%area_ha !root mass (kg = kg/ha * ha)
        depth = 0.
        rm_fract = 0.
        do jj=1,soil(j)%nly
          !bottom of soil layer (mm)
          depth = depth + soil(j)%phys(jj)%thick
          !root mass in layer (kg)
          if(rd >= depth) then
            rm_layer = rm * (soil(j)%phys(jj)%thick / rd) !kg
          else
            rm_layer = rm * ((soil(j)%phys(jj)%thick - (depth-rd)) / rd) !kg
          endif
          if(rm_layer > 0) then
            rm_fract(jj) = rm_layer / rm 
					endif
        enddo !go to next soil layer
         
        !determine the constituent mass uptake in each layer
        do ics=1,cs_db%num_cs
          do jj=1,soil(j)%nly 
            !uptake mass (kg/ha) for layer
            uptake_mass = cs_uptake_kg(idp,ics) * rm_fract(jj)
            !limit uptake to constituent mass that is present in soil water
            if(uptake_mass > cs_soil(j)%ly(jj)%cs(ics)) then
              uptake_mass = cs_soil(j)%ly(jj)%cs(ics)
            endif
            !store uptake mass in daily mass balance array
            hcsb_d(j)%cs(ics)%uptk = hcsb_d(j)%cs(ics)%uptk + uptake_mass !kg/ha
            !remove constituent mass from soil water
            cs_soil(j)%ly(jj)%cs(ics) = cs_soil(j)%ly(jj)%cs(ics) - uptake_mass !kg/ha
          enddo !go to next constituent
        enddo !go to next soil layer
        
      endif !check for rooting depth
      
      
      return
      end subroutine cs_uptake
      