      !ICRA copy of res_pesticide_output
      
      subroutine res_pollutant_output(j)
    
      use time_module
      use basin_module
      use constituent_mass_module
      use hydrograph_module, only : sp_ob1, ob
      use res_pollutant_module
      
      implicit none
      
      integer :: ipoll                         !            |
      integer :: j
      integer :: iob
      real :: const
                         
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs HRU variables on daily, monthly and annual time steps
     
      iob = sp_ob1%res + j - 1
          
      !! print balance for each pollutant
      do ipoll = 1, cs_db%num_poll
          
       respoll_m(j)%poll(ipoll) = respoll_m(j)%poll(ipoll) + respoll_d(j)%poll(ipoll)

      !! daily print
        if (pco%day_print == "y" .and. pco%int_day_cur == pco%int_day) then
          if (pco%poll%d == "y") then
             write (2828,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, cs_db%poll(ipoll), &
                respoll_d(j)%poll(ipoll)   !! pollutant balance
             if (pco%csvout == "y") then
               write (2836,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                    cs_db%poll(ipoll), respoll_d(j)%poll(ipoll)
             end if
          end if
        end if
        !! zero daily output
        
        
        !! check end of month
        if (time%end_mo == 1) then
          respoll_y(j)%poll(ipoll) = respoll_y(j)%poll(ipoll) + respoll_m(j)%poll(ipoll)
          const = float (ndays(time%mo + 1) - ndays(time%mo))
          respoll_m(j)%poll(ipoll) = respoll_m(j)%poll(ipoll) // const

          !! monthly print
           if (pco%poll%m == "y") then
             write (2829,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, cs_db%poll(ipoll), &
                respoll_m(j)%poll(ipoll)
               if (pco%csvout == "y") then
                 write (2837,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                    cs_db%poll(ipoll), respoll_m(j)%poll(ipoll)
               end if
           end if
          
          respoll_m(j)%poll(ipoll) = res_pollbz
        end if
        
        !! check end of year
        if (time%end_yr == 1) then
          respoll_a(j)%poll(ipoll) = respoll_a(j)%poll(ipoll) + respoll_y(j)%poll(ipoll)
          const = time%day_end_yr
          respoll_y(j)%poll(ipoll) = respoll_y(j)%poll(ipoll) // const

          !! yearly print
           if (time%end_yr == 1 .and. pco%poll%y == "y") then
             write (2830,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, cs_db%poll(ipoll), &
                respoll_y(j)%poll(ipoll)
               if (pco%csvout == "y") then
                 write (2838,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                    cs_db%poll(ipoll), respoll_y(j)%poll(ipoll)
               end if
           end if
           
        end if
        
!!!!! average annual print
         if (time%end_sim == 1 .and. pco%poll%a == "y") then
           respoll_a(j)%poll(ipoll) = respoll_a(j)%poll(ipoll) / time%yrs_prt
           respoll_a(j)%poll(ipoll) = respoll_a(j)%poll(ipoll) // time%days_prt
           write (2831,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, cs_db%poll(ipoll), &
            respoll_a(j)%poll(ipoll)
           if (pco%csvout == "y") then
             write (28239,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                cs_db%poll(ipoll), respoll_a(j)%poll(ipoll)
           end if
           respoll_a(j)%poll(ipoll) = res_pollbz
         end if

      end do    !pollutant loop
      return
      
100   format (4i6,2i8,2x,2a,14e12.4)      

      end subroutine res_pollutant_output