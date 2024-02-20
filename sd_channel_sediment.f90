      subroutine sd_channel_sediment (ts_int)

      use sd_channel_module
      use channel_velocity_module
      use basin_module
      use hydrograph_module
      use constituent_mass_module
      use conditional_module
      use channel_data_module
      use channel_module
      use ch_pesticide_module
      use climate_module
      use water_body_module
      use time_module
    
      implicit none     
    
      integer, intent (in) :: ts_int  !none          |number of time steps in a day
      real :: ts_hr                   !hours         |time step
      
      !real :: rcharea                !m^2           |cross-sectional area of flow
      integer :: isd_db               !              |
      real :: erode_btm               !cm            |
      real :: erode_bank              !cm            |meander cut on one side
      real :: erode_bank_cut          !cm            |widening caused by downcutting (both sides)
      real :: deg_btm                 !tons          |bottom erosion
      real :: deg_bank                !tons          |bank erosion
      real :: sedout                  !mg		     |sediment out of waterway channel
      real :: washld                  !tons          |wash load  
      real :: bedld                   !tons          |bed load
      real :: dep                     !tons          |deposition
      real :: hc_sed                  !tons          |headcut erosion 
      real :: e_btm                   !cm            |erosion on bottom of channel at each time step
      real :: dep_btm                 !cm            |deposition on bottom of channel
      real :: pr_ratio                !              |
      real  :: tw                     !              |
      real :: shear_btm_cr            !              |
      real :: shear_btm               !              |
      real :: shear_bank_cr           !              | 
      real :: qmm                     !              | 
      real :: qh                      !              | 
      real :: hc                      !m/yr          |head cut advance
      integer :: max                  !              |
      real :: chns                    !              |
      integer :: ihval                !none          |counter 
      real :: perim_bed               !              |
      real :: perim_bank              !              |
      real :: s_bank                  !              |
      real :: shear_bank              !              |
      real :: shear_bank_adj          !              | 
      real :: e_bank                  !              | 
           
      ich = isdch
      isd_db = sd_dat(ich)%hyd
      iwst = ob(icmd)%wst
      erode_btm = 0.
      erode_bank = 0.
      erode_bank_cut = 0.
      deg_btm = 0.
      deg_bank = 0.
      sedout = 0.
      washld = 0.
      bedld = 0.
      dep = 0.
      hc = 0.
      hc_sed = 0.
      
      ts_hr = 24. / float (ts_int)
      
      !! adjust peak rate for headcut advance -also adjusts CEAP gully from
      !! edge-of-field to trib (assuming rectangular shape and constant tc)
      if (sd_ch(ich)%hc_erod > 1.e-6) then
        pr_ratio = (sd_ch(ich)%chl - sd_ch(ich)%hc_len / 1000.) / sd_ch(ich)%chl
        pr_ratio = Max(pr_ratio, 0.)
        
        !! new q*qp (m3 * m3/s) equation for entire runoff event
        qmm = ht1%flo / (10. * ob(icmd)%area_ha)
        if (qmm > 3.) then
          qh = (ht1%flo / 86400.) ** .5 * sd_ch(ich)%hc_hgt ** .225
          hc = sd_ch(ich)%hc_co * qh            !m per event
          hc = Max(hc, 0.)
          sd_ch(ich)%hc_len = sd_ch(ich)%hc_len + hc
          if (sd_ch(ich)%hc_len > sd_ch(ich)%chl * 1000.) then
            hc = hc - (sd_ch(ich)%hc_len - sd_ch(ich)%chl * 1000.)
            sd_ch(ich)%hc_len = sd_ch(ich)%chl * 1000.
          end if
            
          !! compute sediment yield from headcut- assume bd = 1.2 t/m3
          !! assume channel dimensions are same as data file
          hc_sed = hc * sd_ch(ich)%chw * sd_ch(ich)%chd * 1.2
        end if
      end if
        
      
        !! break hydrograph into maxint segments and compute deg at each flow increment
        do ihval = 1, ts_int
          !! calc critical shear and shear on bottom of channel
          shear_btm_cr = sd_ch(ich)%d50
          shear_btm = 9800. * hyd_rad(ihval) * sd_ch(ich)%chs   !! Pa = N/m^2 * m * m/m
            
          !! degradation of the bank (widening)
          perim_bank = 2. * ((sd_ch(ich)%chd ** 2) * (1. + sd_ch(ich)%chss ** 2)) ** 0.5
          perim_bed = sd_ch(ich)%chw
          tw = perim_bed + 2. * sd_ch(ich)%chss * rchdep
          s_bank = 1.77 * (perim_bed / perim_bank + 1.5) ** - 1.4
          !! assume bank shear is 75% of bottom shear
          shear_bank = shear_btm * 0.75     !sd_ch(ich)%shear_bnk * s_bank * (tw * perim_bed) / (2. * perim_bank)
          if (sd_ch(ich)%ch_clay >= 10.) then
            chns = .0156
          else
            chns = (sd_ch(ich)%d50 / 25.4) ** .16666 / 39.
          end if
          shear_bank_adj = shear_bank * (1. - sd_ch(ich)%cov)      !* (chns / sd_chd(isd_db)%chn) ** 2
          shear_bank_cr = 0.493 * 10. ** (.0182 * sd_ch(ich)%ch_clay)
          e_bank = 0.
          if (shear_bank_adj > shear_bank_cr) then
            e_bank = ts_hr * sd_ch(ich)%cherod * (shear_bank_adj - shear_bank_cr)    !! cm = hr * cm/hr/Pa * Pa
            erode_bank = erode_bank + e_bank
            !! calc mass of sediment eroded -> t = cm * m/100cm * width (m) * length (km) * 1000 m/km * bd (t/m3)
            !! apply to only one side (perim_bank / 2.)
            deg_bank = deg_bank + 10. * e_bank * perim_bank / 2. * sd_ch(ich)%chl * sd_ch(ich)%ch_bd
          end if
              
          !! no downcutting below equilibrium slope
          e_btm = 0.
          erode_bank_cut = 0.
          if (sd_ch(ich)%chs > 0.000001) then       ! sd_ch(ich)%chseq) then
            !! if bottom shear > d50 -> downcut - widen to maintain width depth ratio
            if (shear_btm > shear_btm_cr) then
              e_btm = ts_hr *  sd_ch(ich)%cherod * (shear_btm - shear_btm_cr)    !! cm = hr * cm/hr/Pa * Pa
              !! if downcutting - check width depth ratio to see if widens
              !if (sd_ch(ich)%chw / sd_ch(ich)%chd < sd_ch(ich)%wd_rto) then
              !  erode_bank_cut = e_btm * sd_ch(ich)%wd_rto
                !! appy to both bank sides
              !  deg_bank = deg_bank + 10. * erode_bank_cut * perim_bank * sd_ch(ich)%chl * sd_ch(ich)%ch_bd
              !end if
              erode_btm = erode_btm + e_btm
              !! calc mass of sediment eroded -> t = cm * m/100cm * width (m) * length (km) * 1000 m/km * bd (t/m3)
              deg_btm = deg_btm + 10. * e_btm * perim_bed * sd_ch(ich)%chl * sd_ch(ich)%ch_bd
            end if
          end if

        end do    ! ihval
        
          erode_btm = max (0., erode_btm)
          erode_bank = max (0., erode_bank)
          erode_bank_cut = max (0., erode_bank_cut)
          
          !! adjust for incoming bedload and compute deposition
          !! assume bedload is deposited
          dep = 0.  ! sd_ch(ich)%bedldcoef * ht1%sed
          dep_btm = dep / (10. * perim_bed * sd_ch(ich)%chl * sd_ch(ich)%ch_bd)
          erode_btm = erode_btm ! - dep_btm      !don't add in all bedload (most will be transported out)
          sd_ch(ich)%chd = sd_ch(ich)%chd + erode_btm / 100.
          if (sd_ch(ich)%chd < 0.) then
            !! stream is completely filled in
            sd_ch(ich)%chd = 0.01
          end if
          
          sd_ch(ich)%chw = sd_ch(ich)%chw + erode_bank / 100. + 2. * erode_bank_cut / 100.
          sd_ch(ich)%chs = sd_ch(ich)%chs - (erode_btm / 100.) / (sd_ch(ich)%chl * 1000.)
          sd_ch(ich)%chs = max (0.000001, sd_ch(ich)%chs)
      
      !! output channel morphology
      chsd_d(ich)%sed_in = ob(icmd)%hin%sed
      chsd_d(ich)%sed_out = sedout
      chsd_d(ich)%sed_stor = ch_stor(ich)%sed
      if (sedout > 2000.) then      !***jga
        dep = bedld
      end if
      chsd_d(ich)%washld = washld
      chsd_d(ich)%bedld = bedld
      chsd_d(ich)%dep = dep
      chsd_d(ich)%deg_btm = deg_btm
      chsd_d(ich)%deg_bank = deg_bank
      chsd_d(ich)%hc_sed = hc_sed
      chsd_d(ich)%width = sd_ch(ich)%chw
      chsd_d(ich)%depth = sd_ch(ich)%chd
      chsd_d(ich)%slope = sd_ch(ich)%chs
      chsd_d(ich)%deg_btm_m = erode_btm
      chsd_d(ich)%deg_bank_m = erode_bank
      chsd_d(ich)%hc_m = hc
      
        !end if

      !! compute sediment leaving the channel
	  washld = ht1%sed  ! (1. - sd_ch(ich)%bedldcoef) * ht1%sed
	  sedout = washld + hc_sed + deg_btm + deg_bank
      dep = ht1%sed - sedout
      dep = max (0., dep)

      return
      
      end subroutine sd_channel_sediment