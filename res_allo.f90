      subroutine res_allo
      
      use reservoir_module
      use reservoir_data_module
      use res_pesticide_module
      use hydrograph_module
      use constituent_mass_module
      use water_body_module
      use res_pollutant_module  !ICRA
      
      implicit none     

      integer :: ires           !             |
      integer :: mres           !             |
      
      mres = sp_ob%res
      allocate (res(0:mres))
      allocate (res_om_init(0:mres))
      allocate (res_ob(0:mres))
      allocate (res_in_d(mres))
      allocate (res_in_m(mres))
      allocate (res_in_y(mres))
      allocate (res_in_a(mres))
      allocate (res_out_d(mres))
      allocate (res_out_m(mres))
      allocate (res_out_y(mres))
      allocate (res_out_a(mres))
      allocate (res_wat_d(mres))
      allocate (res_wat_m(mres))
      allocate (res_wat_y(mres))
      allocate (res_wat_a(mres))
      allocate (res_water(mres))
      allocate (res_benthic(mres))
      allocate (respst_d(mres))
      allocate (respst_m(mres))
      allocate (respst_y(mres))
      allocate (respst_a(mres))


      allocate (respoll_d(mres))  !ICRA
      allocate (respoll_m(mres))  !ICRA
      allocate (respoll_y(mres))  !ICRA
      allocate (respoll_a(mres))  !ICRA

      
      if (cs_db%num_tot > 0) then
        do ires = 1, sp_ob%res
          if (cs_db%num_pests > 0) then 
            allocate (res_water(ires)%pest(cs_db%num_pests))
            allocate (res_benthic(ires)%pest(cs_db%num_pests))
            allocate (res_ob(ires)%aq_mix(cs_db%num_pests))
            allocate (respst_d(ires)%pest(cs_db%num_pests))
            allocate (respst_m(ires)%pest(cs_db%num_pests))
            allocate (respst_y(ires)%pest(cs_db%num_pests))
            allocate (respst_a(ires)%pest(cs_db%num_pests))
            allocate (res_water(ires)%path(cs_db%num_paths))
          end if 
          if (cs_db%num_poll > 0) then  !ICRA
            allocate (res_water(ires)%poll(cs_db%num_poll))
            allocate (res_benthic(ires)%poll(cs_db%num_poll))
            allocate (res_ob(ires)%aq_mix_poll(cs_db%num_poll))
            allocate (respoll_d(ires)%poll(cs_db%num_poll))
            allocate (respoll_m(ires)%poll(cs_db%num_poll))
            allocate (respoll_y(ires)%poll(cs_db%num_poll))
            allocate (respoll_a(ires)%poll(cs_db%num_poll))
          end if 
          allocate (res_benthic(ires)%path(cs_db%num_paths))
          allocate (res_water(ires)%hmet(cs_db%num_metals))
          allocate (res_benthic(ires)%hmet(cs_db%num_metals))
          allocate (res_water(ires)%salt(cs_db%num_salts))
          allocate (res_benthic(ires)%salt(cs_db%num_salts))
        end do
        if (cs_db%num_pests > 0) then
          allocate (brespst_d%pest(cs_db%num_pests))
          allocate (brespst_m%pest(cs_db%num_pests))
          allocate (brespst_y%pest(cs_db%num_pests))
          allocate (brespst_a%pest(cs_db%num_pests))
        end if

        !ICRA
        if (cs_db%num_poll > 0) then
          allocate (brespoll_d%poll(cs_db%num_poll))
          allocate (brespoll_m%poll(cs_db%num_poll))
          allocate (brespoll_y%poll(cs_db%num_poll))
          allocate (brespoll_a%poll(cs_db%num_poll))
        end if
      end if

      return
      end subroutine res_allo