     !ICRA copy of cha_pesticide_output 
     
     
     subroutine cha_pollutant_output(jrch)
    
      use time_module
      use basin_module
      use constituent_mass_module
      use hydrograph_module, only : sp_ob1, ob
      use ch_pollutant_module

      
      implicit none
      
      integer, intent (in) :: jrch             !            |
      integer :: ipoll                         !            |
      integer :: j
      integer :: iob
      real :: const
                         
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs HRU variables on daily, monthly and annual time steps


      j = jrch  !!! nbs
      
      iob = sp_ob1%chandeg + j - 1
          
      !! print balance for each pollutant
      do ipoll = 1, cs_db%num_poll
          
       chpoll_m(j)%poll(ipoll) = chpoll_m(j)%poll(ipoll) + chpoll_d(j)%poll(ipoll)

      !! daily print
        if (pco%day_print == "y" .and. pco%int_day_cur == pco%int_day) then
          if (pco%poll%d == "y") then
             write (2824,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, cs_db%poll(ipoll), &
             chpoll_d(j)%poll(ipoll)   !! pollutant balance
             if (pco%csvout == "y") then
               write (2832,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                 cs_db%poll(ipoll), chpoll_d(j)%poll(ipoll)
             end if
          end if
        end if
        !! zero daily output
        
        
        !! check end of month
        if (time%end_mo == 1) then
          chpoll_y(j)%poll(ipoll) = chpoll_y(j)%poll(ipoll) + chpoll_m(j)%poll(ipoll)
          const = float (ndays(time%mo + 1) - ndays(time%mo))
          chpoll_m(j)%poll(ipoll) = chpoll_m(j)%poll(ipoll) // const

          !! monthly print
           if (pco%poll%m == "y") then
             write (2825,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, cs_db%poll(ipoll), &
               chpoll_m(j)%poll(ipoll)
               if (pco%csvout == "y") then
                 write (2833,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                   cs_db%poll(ipoll), chpoll_m(j)%poll(ipoll)
               end if
           end if
          
          chpoll_m(j)%poll(ipoll) = ch_pollbz
        end if
        
        !! check end of year
        if (time%end_yr == 1) then
          chpoll_a(j)%poll(ipoll) = chpoll_a(j)%poll(ipoll) + chpoll_y(j)%poll(ipoll)
          const = time%day_end_yr
          chpoll_y(j)%poll(ipoll) = chpoll_y(j)%poll(ipoll) // const

          !! yearly print
           if (time%end_yr == 1 .and. pco%poll%y == "y") then
             write (2826,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, cs_db%poll(ipoll), &
               chpoll_y(j)%poll(ipoll)
               if (pco%csvout == "y") then
                 write (2834,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                   cs_db%poll(ipoll), chpoll_y(j)%poll(ipoll)
               end if
           end if
           
        end if
        
!!!!! average annual print
         if (time%end_sim == 1 .and. pco%poll%a == "y") then
           chpoll_a(j)%poll(ipoll) = chpoll_a(j)%poll(ipoll) / time%yrs_prt
           chpoll_a(j)%poll(ipoll) = chpoll_a(j)%poll(ipoll) // time%yrs_prt
           write (2827,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, cs_db%poll(ipoll), &
             chpoll_a(j)%poll(ipoll)
           if (pco%csvout == "y") then
             write (2835,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
               cs_db%poll(ipoll), chpoll_a(j)%poll(ipoll)
           end if
           chpoll_a(j)%poll(ipoll) = ch_pollbz
         end if

      end do    !pollutant loop
      return
      
100   format (4i6,2i8,2x,2a,14e12.4)      

      end subroutine cha_pollutant_output