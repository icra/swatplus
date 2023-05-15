      subroutine ch_rtpoll
      
!!     ~ ~ ~ PURPOSE ~ ~ ~
!!     this subroutine computes the daily stream pollutants balance
!!     (soluble and sorbed)     

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    chpoll_conc(:) |mg/(m**3)     |initial pollutant concentration in reach
!!    chpoll_koc(:)  |m**3/g        |pollutant partition coefficient between
!!                                 |water and sediment in reach
!!    chpoll_mix(:)  |m/day         |mixing velocity (diffusion/dispersion) for
!!                                 |pollutant in reach
!!    chpoll_rea(:)  |1/day         |pollutant reaction coefficient in reach
!!    chpoll_rsp(:)  |m/day         |resuspension velocity in reach for pollutant
!!                                 |sorbed to sediment
!!    chpoll_stl(:)  |m/day         |settling velocity in reach for pollutant
!!                                 |sorbed to sediment
!!    chpoll_vol(:)  |m/day         |pollutant volatilization coefficient in 
!!                                 |reach
!!    drift(:)      |kg            |amount of pollutant drifting onto main
!!                                 |channel in subbasin
!!    rchdep        |m             |depth of flow on day
!!    rchwtr        |m^3 H2O       |water stored in reach at beginning of day
!!    sedpoll_rea(:) |1/day         |pollutant reaction coefficient in river bed
!!                                 |sediment
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bury        |mg poll        |loss of pollutant from active sediment layer
!!                               |by burial
!!    difus       |mg poll        |diffusion of pollutant from sediment to reach
!!    reactb      |mg poll        |amount of pollutant in sediment that is lost
!!                               |through reactions
!!    reactw      |mg poll        |amount of pollutant in reach that is lost
!!                               |through reactions
!!    resuspoll    |mg poll        |amount of pollutant moving from sediment to
!!                               |reach due to resuspension
!!    setlpoll     |mg poll        |amount of pollutant moving from water to
!!                               |sediment due to settling
!!    solpesto    |mg poll/m^3    |soluble pollutant concentration in outflow
!!                               |on day
!!    sorpesto    |mg poll/m^3    |sorbed pollutant concentration in outflow
!!                               |on day
!!    volatpoll    |mg poll        |amount of pollutant in reach lost by
!!                               |volatilization
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    chpollmass   |mg poll        |mass of pollutant in reach
!!    depth       |m             |depth of water in reach
!!    fd2         |
!!    frsol       |none          |fraction of pollutant in reach that is soluble
!!    frsrb       |none          |fraction of pollutant in reach that is sorbed
!!    jrch        |none          |reach number
!!    pollin       |mg poll        |total pollutant transported into reach
!!                               |during time step
!!    sedcon      |g/m^3         |sediment concentration
!!    sedpollmass  |mg poll        |mass of pollutant in bed sediment
!!    solpollin    |mg poll        |soluble pollutant entering reach during 
!!                               |time step
!!    sorpollin    |mg poll        |sorbed pollutant entering reach during
!!                               |time step
!!    tday        |days          |flow duration
!!    wtrin       |m^3 H2O       |volume of water entering reach during time
!!                               |step
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Abs

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      
      use channel_data_module
      use channel_module
      use sd_channel_module
      use ch_pollutant_module
      
      use hydrograph_module, only : ob, jrch, ht1, ch_stor, sp_ob1
      use constituent_mass_module
      use pollutants_data_module

      implicit none
      
      integer :: ipoll          !none                   |pollutant counter - sequential
      integer :: jpoll           !none                   |pollutant counter from data base
      integer :: ipseq          !none                   |sequential basin pollutant number
      integer :: ipdb           !none                   |seqential pollutant number of daughter pollutant
      integer :: imeta          !none                   |pollutant metabolite counter
      integer :: num_poll       !none                   |number of pollutants
      real :: mol_wt_rto        !ratio                  |molecular weight ratio of duaghter to parent pollutant
      real :: pollin             !mg poll                 |total pollutant transported into reach during time step
      real :: kd                !(mg/kg)/(mg/L)         |koc * carbon
      real :: depth             !m             |depth of water in reach
      real :: chpollmass         !mg poll        |mass of pollutant in reach
      real :: sedpollmass        !mg poll        |mass of pollutant in bed sediment
      real :: fd2               !units         |description
      real :: solmax            !units         |description
      real :: sedcon            !g/m^3         |sediment concentration 
      real :: tday              !none          |flow duration (fraction of 24 hr)
      real :: rchwtr            !m^3 H2O       |water stored in reach at beginning of day
      real :: por               !none          |porosity of bottom sediments
      real :: poll_init         !mg            |amount of pollutant before decay
      real :: poll_end          !mg            |amount of pollutant after decay

      !! zero outputs
      chpoll_d(jrch) = chpollz
      num_poll = cs_db%num_poll

      
      !! initialize depth of water for pollutant calculations
      if (rchdep < 0.01) then
        depth = .01
      else
        depth = rchdep
      endif

      !! 1236 format ("Found:", 2x, a)

      do ipoll = 1, num_poll
        jpoll = ipoll

        !! volume of water entering reach and stored in reach
        wtrin = ht1%flo + ch_stor(jrch)%flo

        !! pollutant transported into reach during day
        pollin = hcs1%poll(ipoll) 

        !! calculate mass of pollutant in reach
        chpollmass = pollin + ch_water(jrch)%poll(ipoll)

        !! calculate mass of pollutant in bed sediment
        sedpollmass = ch_benthic(jrch)%poll(ipoll)

        if (jrch == 44 .or. jrch == 5 .or. jrch == 4 .or. jrch == 17) then
          write (*,*) ob((sp_ob1%chandeg) + jrch - 1)%name, polldb(ipoll)%name, chpollmass, sedpollmass
        endif


        if (chpollmass + sedpollmass < 1.e-12) then

          ch_water(jrch)%poll(ipoll) = 0.
          ch_benthic(jrch)%poll(ipoll) = 0.

        end if

        if (chpollmass + sedpollmass < 1.e-12) return


        !!in-stream processes
        if (wtrin / 86400. > 1.e-9) then
          !! calculate sediment concentration
          sedcon = ht1%sed / wtrin * 1.e6
          
          !! set kd
          !kd = polldb(ipoll)%koc * sd_ch(jrch)%carbon / 100.

          !! calculate fraction of soluble and sorbed pollutant
          !frsol = 1. / (1. + kd * sedcon)
          !frsrb = 1. - frsol

          !! ASSUME DENSITY=2.6E6; KD2=KD1
          !por = 1. - sd_ch(jrch)%ch_bd / 2.65
          !fd2 = 1. / (por + kd)

          !! calculate flow duration
          tday = rttime / 24.0
          if (tday > 1.0) tday = 1.0
          tday = 1.0

          !! calculate amount of pollutant that undergoes chemical or biological degradation on day in reach
          poll_init = chpollmass
          if (poll_init > 1.e-12) then
            poll_end = chpollmass * pollcp(ipoll)%decay_a
            chpollmass = poll_end
            chpoll%poll(ipoll)%react = poll_init - poll_end
            !! add decay to daughter pollutants
            do imeta = 1, pollcp(ipoll)%num_metab
              ipseq = pollcp(ipoll)%daughter(imeta)%num
              ipdb = ipseq
              mol_wt_rto = polldb(ipdb)%mol_wt / polldb(ipoll)%mol_wt
              chpoll_d(jrch)%poll(ipseq)%metab = chpoll_d(jrch)%poll(ipseq)%metab + chpoll%poll(ipoll)%react *     &
                                           pollcp(ipoll)%daughter(imeta)%aq_fr * mol_wt_rto
              hcs1%poll(ipseq) = hcs1%poll(ipseq) + chpoll_d(jrch)%poll(ipseq)%metab
            end do
          end if

          !! calculate amount of pollutant that volatilizes from reach
          chpoll%poll(ipoll)%volat = polldb(jpoll)%aq_volat * frsol * chpollmass * tday / depth



          if (chpoll%poll(ipoll)%volat > frsol * chpollmass) then
            chpoll%poll(ipoll)%volat = frsol * chpollmass 
            chpollmass = chpollmass - chpoll%poll(ipoll)%volat
          else
            chpollmass = chpollmass - chpoll%poll(ipoll)%volat
          end if


          !! calculate amount of pollutant removed from reach by settling
          chpoll%poll(ipoll)%settle = polldb(jpoll)%aq_settle * frsrb * chpollmass * tday / depth
          if (chpoll%poll(ipoll)%settle >  frsrb * chpollmass) then
            chpoll%poll(ipoll)%settle = frsrb * chpollmass
            chpollmass = chpollmass - chpoll%poll(ipoll)%settle
          else
            chpollmass = chpollmass - chpoll%poll(ipoll)%settle
          end if
          sedpollmass = sedpollmass + chpoll%poll(ipoll)%settle

          !! calculate resuspension of pollutant in reach
          chpoll%poll(ipoll)%resus = polldb(jpoll)%aq_resus * sedpollmass * tday / depth

          if (chpoll%poll(ipoll)%resus > sedpollmass) then
            chpoll%poll(ipoll)%resus = sedpollmass
            sedpollmass = 0.
          else
            sedpollmass = sedpollmass - chpoll%poll(ipoll)%resus
          end if
          


          chpollmass = chpollmass + chpoll%poll(ipoll)%resus

          !! calculate diffusion of pollutant between reach and sediment
          !chpoll%poll(ipoll)%difus = sd_ch(jrch)%aq_mix(ipoll) * (fd2 * sedpollmass - frsol * chpollmass) * tday / depth
          !if (chpoll%poll(ipoll)%difus > 0.) then
          !  if (chpoll%poll(ipoll)%difus > sedpollmass) then
          !    chpoll%poll(ipoll)%difus = sedpollmass
          !    sedpollmass = 0.
          !  else
          !    sedpollmass = sedpollmass - Abs(chpoll%poll(ipoll)%difus)
          !  end if
          !  chpollmass = chpollmass + Abs(chpoll%poll(ipoll)%difus)
          !else
          !  if (Abs(chpoll%poll(ipoll)%difus) > chpollmass) then
          !    chpoll%poll(ipoll)%difus = -chpollmass
          !    chpollmass = 0.
          !  else
          !    chpollmass = chpollmass - Abs(chpoll%poll(ipoll)%difus)
          !  end if
          !  sedpollmass = sedpollmass + Abs(chpoll%poll(ipoll)%difus)
          !end if

          !! calculate removal of pollutant from active sediment layer by burial
          chpoll%poll(ipoll)%bury = polldb(jpoll)%ben_bury * sedpollmass / polldb(jpoll)%ben_act_dep
          if (chpoll%poll(ipoll)%bury > sedpollmass) then
            chpoll%poll(ipoll)%bury = sedpollmass
            sedpollmass = 0.
          else
            sedpollmass = sedpollmass - chpoll%poll(ipoll)%bury
          end if

          

          !! verify that water concentration is at or below solubility
          solmax = polldb(jpoll)%solub * wtrin
          if (solmax < chpollmass * frsol) then
            sedpollmass = sedpollmass + (chpollmass * frsol - solmax)
            chpollmass = chpollmass - (chpollmass * frsol - solmax)
          end if
        
        else   
          !!insignificant flow
          sedpollmass = sedpollmass + chpollmass
          chpollmass = 0.
        end if

        !! benthic processes
        !! calculate loss of pollutant from bed sediments by reaction
        poll_init = sedpollmass
        if (poll_init > 1.e-12) then
          poll_end = sedpollmass * pollcp(jpoll)%decay_b
          sedpollmass = poll_end
          chpoll%poll(ipoll)%react_bot = poll_init - poll_end
          !! add decay to daughter pollicides
          do imeta = 1, pollcp(jpoll)%num_metab
            ipseq = pollcp(jpoll)%daughter(imeta)%num
            ipdb = ipseq
            mol_wt_rto = polldb(ipdb)%mol_wt / polldb(jpoll)%mol_wt
            chpoll_d(jrch)%poll(ipseq)%metab_bot = chpoll_d(jrch)%poll(ipseq)%metab + chpoll%poll(ipoll)%react_bot *     &
                                           pollcp(jpoll)%daughter(imeta)%ben_fr * mol_wt_rto
            ch_benthic(jrch)%poll(ipseq) = ch_benthic(jrch)%poll(ipseq) + chpoll_d(jrch)%poll(ipseq)%metab
          end do
        end if

        !! set new pollutant mass of (in + store) after processes
        if (wtrin > 1.e-6) then
          hcs1%poll(ipoll) = chpollmass
        else
          sedpollmass = sedpollmass + chpollmass
        end if
        ch_benthic(jrch)%poll(ipoll) = sedpollmass

      end do

      return
      end subroutine ch_rtpoll