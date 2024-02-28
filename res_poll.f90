      subroutine res_poll (jres)

!!    ICRA
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes the lake hydrologic pollutant balance.

      use reservoir_data_module
      use reservoir_module
      use hydrograph_module, only : res, ob, ht2, wbody
      use constituent_mass_module
      use water_body_module


      use pesticide_data_module
      use res_pesticide_module
      use pollutants_data_module  !ICRA
      use res_pollutant_module    !ICRA
      
      implicit none      
      
      real :: tpoll1                !mg poll       |amount of pollutant in water
      real :: tpoll2                !mg poll        |amount of pollutant in benthic sediment
      real :: kd                    !(mg/kg)/(mg/L) |koc * carbon
      real :: fd1                   !              |frac of soluble pollutant in water column
      real :: fd2                   !              |frac of sorbed pollutant in water column
      real :: fp1                   !              |frac of soluble pollutant in benthic column
      real :: fp2                   !              |frac of sorbed pollutant in benthic column
      real :: depth                 !              |average depth of reservoir
      real :: bedvol                !m^3           |volume of river bed sediment
      real :: solpoll              !mg poll         |soluble pollutant transported out of reservoir
      real :: sorpoll              !mg poll        |sorbed pollutant transported out of reservoir
      real :: sedmass_watervol      !kg/L or t/m3  |sediment mass divided by water volume in water and benthic
      real :: poll_init             !mg            |amount of pollutant before decay
      real :: poll_end              !mg            |amount of pollutant after decay
      real :: mol_wt_rto        !ratio      |molecular weight ratio of duaghter to parent pollutant
      integer :: ipseq          !none       |sequential basin pollutant number
      integer :: ipdb           !none       |seqential pollutant number of daughter pollutant
      integer :: imeta          !none       |pollutant metabolite counter
      integer :: jres               !none          |reservoir number  
      integer :: ipoll               !none          |counter
      integer :: icmd               !none          |
      integer :: jsed               !none          |counter
      integer :: idb                !none          |

      real :: tpoll1_b              !none          |ICRA aux
      real :: tpoll2_b              !none          |ICRA aux

      
      
      if (res(jres)%flo > 1.) then

          
      do ipoll = 1, cs_db%num_poll
        icmd = res_ob(jres)%ob
        idb = ob(icmd)%props
        jsed = res_dat(idb)%sed
        respoll_d(jres)%poll(ipoll)%tot_in = obcs(icmd)%hin(1)%poll(ipoll) 

        tpoll1 = obcs(icmd)%hin(1)%poll(ipoll) + res_water(jres)%poll(ipoll)

        bedvol = 1000. * res_wat_d(jres)%area_ha * polldb(ipoll)%ben_act_dep + .01
        tpoll2 = res_benthic(jres)%poll(ipoll) * bedvol

        tpoll1_b = tpoll1
        tpoll2_b = tpoll2

        !! calculate average depth of reservoir
        depth = res(jres)%flo / (res_wat_d(jres)%area_ha * 10000.)
        !! sor conc/sol conc = Koc * frac_oc = Kd -> (sor mass/mass sed) / (sol mass/mass water) = Kd
        !! -> sor mass/sol mass = Kd * (kg sed)/(L water) --> sol mass/tot mass = 1 / (1 + Kd * (kg sed)/(L water))
        !! water column --> kg sed/L water = t/m3 = t / (m3 - (t * m3/t)) --> sedvol = sed/particle density(2.65)
        sedmass_watervol = (res(jres)%sed) / (res(jres)%flo - (res(jres)%sed / 2.65))
        
        !kd = polldb(ipoll)%koc * res_sed(jsed)%carbon / 100.
        kd = polldb(ipoll)%kow * 3.085e-8    !ICRA

        fd1 = 1. / (1. + kd * sedmass_watervol)
        fd1 = amin1 (1., fd1)
        fp1 = 1. - fd1
        !! assume; fraction organic = 1%;\; por=0.8; density=2.6 t/m^3
        !! benthic layer --> kg sed/L water = t/m3 = bd (t sed/m3 total) / por --> por*total gives volume of water
        sedmass_watervol = res_sed(jsed)%bd / (1. - res_sed(jsed)%bd / 2.65)
        fd2 = 1. / (1. + kd * sedmass_watervol)
        fd2 = amin1 (1., fd2)
        fp2 = 1. - fd2
        
        fd2 = 1. / (.8 + .026 * kd)
        fd2 = amin1 (1., fd2)
        fp2 = 1. - fd2

        !! determine pollutant lost through reactions in water layer
        poll_init = tpoll1
        if (poll_init > 1.e-12) then
          poll_end = tpoll1 * pollcp(ipoll)%decay_a

          tpoll1 = poll_end
          respoll_d(jres)%poll(ipoll)%react = poll_init - poll_end
          !ICRA commented lines
          !! add decay to daughter pollutants
          !do imeta = 1, pollcp(ipoll)%num_metab
          !  ipseq = pollcp(ipoll)%daughter(imeta)%num
          !  ipdb = cs_db%poll_num(ipseq)
          !  mol_wt_rto = polldb(ipdb)%mol_wt / polldb(ipoll)%mol_wt
          !  respoll_d(jres)%poll(ipseq)%metab = respoll_d(jres)%poll(ipseq)%metab + respoll_d(jres)%poll(ipoll)%react *     &
          !                                 pollcp(ipoll)%daughter(imeta)%soil_fr * mol_wt_rto
          !  res_water(jres)%poll(ipseq) = res_water(jres)%poll(ipseq) + respoll_d(jres)%poll(ipseq)%metab
          !end do
        end if
        
        !! determine pollutant lost through volatilization
        volatpoll = polldb(ipoll)%aq_volat * fd1 * tpoll1 / depth
        if (volatpoll > tpoll1) then
          volatpoll = tpoll1
          tpoll1 = 0.
        else
          tpoll1 = tpoll1 - volatpoll
        end if
        respoll_d(jres)%poll(ipoll)%volat = volatpoll

        !! determine amount of pollutant settling to sediment layer
        setlpoll = polldb(ipoll)%aq_settle * fp1 * tpoll1 / depth
        if (setlpoll > tpoll1) then
          setlpoll = tpoll1
          tpoll1 = 0.
          tpoll2 = tpoll2 + setlpoll
        else
          tpoll1 = tpoll1 - setlpoll
          tpoll2 = tpoll2 + setlpoll
        end if
        respoll_d(jres)%poll(ipoll)%settle = setlpoll

        !! determine pollutant resuspended into lake water
        resuspoll = polldb(ipoll)%aq_resus * tpoll2 / polldb(ipoll)%ben_act_dep
        if (resuspoll > tpoll2) then
          resuspoll = tpoll2
          tpoll2 = 0.
          tpoll1 = tpoll1 + resuspoll
        else
          tpoll2 = tpoll2 - resuspoll
          tpoll1 = tpoll1 + resuspoll
        end if
        respoll_d(jres)%poll(ipoll)%resus = resuspoll

        !! determine pollutant diffusing from sediment to water
        difus_poll = res_ob(jres)%aq_mix_poll(ipoll) *                                 &                                
              (fd2 * tpoll2 / polldb(ipoll)%ben_act_dep - fd1 * tpoll1 / depth)
        if (difus_poll > 0.) then
          if (difus_poll > tpoll2) then
            difus_poll = tpoll2
            tpoll2 = 0.
          else
            tpoll2 = tpoll2 - Abs(difus_poll)
          end if
          tpoll1 = tpoll1 + Abs(difus_poll)
        else
          if (Abs(difus_poll) > tpoll1) then
            difus_poll = -tpoll1
            tpoll1 = 0.
          else
            tpoll1 = tpoll1 - Abs(difus_poll)
          end if
          tpoll2 = tpoll2 + Abs(difus_poll)
        end if
        respoll_d(jres)%poll(ipoll)%difus = difus_poll

        !! determine pollutant lost from sediment by reactions
        poll_init = tpoll2
        if (poll_init > 1.e-12) then
          poll_end = tpoll2 * pollcp(ipoll)%decay_b
          tpoll2 = poll_end
          respoll_d(jres)%poll(ipoll)%react_bot = poll_init - poll_end
          !! add decay to daughter pollutants
          !ICRA commented lines
          !do imeta = 1, pollcp(ipoll)%num_metab
          !  ipseq = pollcp(ipoll)%daughter(imeta)%num
          !  ipdb = cs_db%poll_num(ipseq)
          !  mol_wt_rto = polldb(ipdb)%mol_wt / polldb(ipoll)%mol_wt
          !  respoll_d(jres)%poll(ipseq)%metab = respoll_d(jres)%poll(ipseq)%metab + respoll_d(jres)%poll(ipoll)%react *     &
          !                                 pollcp(ipoll)%daughter(imeta)%soil_fr * mol_wt_rto
          !  res_benthic(jres)%poll(ipseq) = res_benthic(jres)%poll(ipseq) + respoll_d(jres)%poll(ipseq)%metab
          !end do
        end if

        !! determine pollutant lost from sediment by burial
        bury_poll = polldb(ipoll)%ben_bury * tpoll2 / polldb(ipoll)%ben_act_dep
        if (bury_poll > tpoll2) then
          bury_poll = tpoll2
          tpoll2 = 0.
        else
          tpoll2 = tpoll2 - bury_poll
        end if
        respoll_d(jres)%poll(ipoll)%bury = bury_poll

        !! calculate soluble pollutant transported out of reservoir
        solpoll = ht2%flo * fd1 * tpoll1 / res(jres)%flo
        if (solpoll > tpoll1) then
          solpoll = tpoll1
          tpoll1 = 0.
        else
          tpoll1 = tpoll1 - solpoll
        end if

        !! calculate sorbed pollutant transported out of reservoir
        sorpoll = ht2%flo * fp1 * tpoll1 / res(jres)%flo
        if (sorpoll > tpoll1) then
          sorpoll = tpoll1
          tpoll1 = 0.
        else
          tpoll1 = tpoll1 - sorpoll
        end if
        respoll_d(jres)%poll(ipoll)%sol_out = solpoll
        respoll_d(jres)%poll(ipoll)%sor_out = sorpoll

        
        !write (*,*) ob(icmd)%name, polldb(ipoll)%name, tpoll1_b, tpoll1, sorpoll + solpoll


        !! update concentration of pollutant in lake water and sediment
        if (tpoll1 < 1.e-10) tpoll1 = 0.0
        if (tpoll2 < 1.e-10) tpoll2 = 0.0
        
        res_water(jres)%poll(ipoll) = tpoll1
        res_benthic(jres)%poll(ipoll) = tpoll2
        
        !ICRA calculate amount of pollutants leaving reservoir
        hcs2%poll(ipoll) = sorpoll + solpoll



      end do
      end if

      return
      end subroutine res_poll