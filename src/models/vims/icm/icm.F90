#include "fabm_driver.h"
!----------------------------------------------------------------------------------
! FABM-ICM is based on the version of SCHISM-ICM written by Zhengui WANG 
! on July, 2022 (git version 947e0ee7da). At present, it only includes the Core 
! module. Other modules will be added in the future model development
!
! @author => Zhengui WANG
! @date => 2022-07-12
! @license => TBD
!----------------------------------------------------------------------------------
!variables in ICM Core module: unit in mg/L
!----------------------------------------------------------------------------------
! 1  PB1   :  Diatom                                     g/m^3
! 2  PB2   :  Green Algae                                g/m^3
! 3  PB3   :  Cyanobacteria                              g/m^3
! 4  RPOC  :  Refractory Particulate Organic Carbon      g/m^3
! 5  LPOC  :  Labile Particulate Organic Carbon          g/m^3
! 6  DOC   :  Dissolved Orgnaic Carbon                   g/m^3
! 7  RPON  :  Refractory Particulate Organic Nitrogen    g/m^3
! 8  LPON  :  Labile Particulate Organic Nitrogen        g/m^3
! 9  DON   :  Dissolved Orgnaic Nitrogen                 g/m^3
! 10 NH4   :  Ammonium Nitrogen                          g/m^3
! 11 NO3   :  Nitrate Nitrogen                           g/m^3
! 12 RPOP  :  Refractory Particulate Organic Phosphorus  g/m^3
! 13 LPOP  :  Labile Particulate Organic Phosphorus      g/m^3
! 14 DOP   :  Dissolved Orgnaic Phosphorus               g/m^3
! 15 PO4   :  Total Phosphate                            g/m^3
! 16 COD   :  Chemical Oxygen Demand                     g/m^3
! 17 DOX   :  Dissolved Oxygen                           g/m^3
!----------------------------------------------------------------------------------

module vims_icm
  use fabm_types
  use fabm_icm_misc
  implicit none
  private

  type,extends(type_base_model),public :: type_vims_icm 
    !variable identifier
    type(type_state_variable_id) :: id_PB1, id_PB2,  id_PB3,  id_RPOC, id_LPOC
    type(type_state_variable_id) :: id_DOC, id_RPON, id_LPON, id_DON,  id_NH4
    type(type_state_variable_id) :: id_NO3, id_RPOP, id_LPOP, id_DOP,  id_PO4
    type(type_state_variable_id) :: id_COD, id_DOX
    type (type_bottom_state_variable_id) :: id_bPOC,id_bPON,id_bPOP

    !dependence 
    type(type_dependency_id) :: id_temp,id_salt,id_dz,id_zr,id_PAR !,id_Ke !,id_PAR,id_TSS
    type(type_surface_dependency_id) :: id_Light0,id_wspd
    type(type_global_dependency_id) :: id_dt
    !type(type_horizontal_dependency_id) :: id_GPM_1

    !diagnostic
    type(type_diagnostic_variable_id) :: id_dPAR

    !model parameters
    integer :: iKe
    real(rk) :: Ke0,KeC,KeS,tss2c
    real(rk),dimension(3) :: alpha,GPM,TGP,PRR,MTB,TMT,KTMT,WSPBS
    real(rk) :: KTGP(3,2),WSPOM(2),WSSED
    real(rk) :: FCP(3,3),FNP(4),FPP(4),FCM(3),FNM(3,4),FPM(3,4)
    real(rk) :: Nit,TNit,KTNit(2),KhDOn,KhNH4n,KhDOox,KhNO3dn
    real(rk),dimension(3) :: KC0,KN0,KP0,KCalg,KNalg,KPalg,TRM,KTRM
    real(rk) :: KCD,TRCOD,KTRCOD,KhCOD
    real(rk),dimension(3) :: KhN,KhP,KhSal,c2chl,n2c,p2c,KhDO,PBmin
    real(rk) :: o2c,o2n,dn2c,an2c,KPO4p,WRea
    real(rk) :: bKC,bKN,bKP,bKTC,bKTN,bKTP
 
  contains  
    procedure :: initialize
    procedure :: do
    !procedure :: do_ppdd
    procedure :: do_column
    procedure :: get_vertical_movement
    procedure :: do_surface
    procedure :: do_bottom
  end type

contains
  
  subroutine initialize(self, configunit)
    class (type_vims_icm), intent(inout), target :: self
    integer, intent(in) :: configunit

    integer :: i,j
    character(len=1) :: a,b

    !----------------------------------------------------------------------------------------------------
    !register ICM model variables
    !----------------------------------------------------------------------------------------------------
    call self%register_state_variable(self%id_PB1, 'PB1', 'mg/L','PB1', 0.1_rk,  minimum=0.0_rk,no_river_dilution=.true.) 
    call self%register_state_variable(self%id_PB2, 'PB2', 'mg/L','PB2', 0.1_rk,  minimum=0.0_rk,no_river_dilution=.true.) 
    call self%register_state_variable(self%id_PB3, 'PB3', 'mg/L','PB3', 0.1_rk,  minimum=0.0_rk,no_river_dilution=.true.) 
    call self%register_state_variable(self%id_RPOC,'RPOC','mg/L','RPOC',0.2_rk,  minimum=0.0_rk,no_river_dilution=.true.) 
    call self%register_state_variable(self%id_LPOC,'LPOC','mg/L','LPOC',0.2_rk,  minimum=0.0_rk,no_river_dilution=.true.) 
    call self%register_state_variable(self%id_DOC, 'DOC', 'mg/L','DOC', 0.3_rk,  minimum=0.0_rk,no_river_dilution=.true.) 
    call self%register_state_variable(self%id_RPON,'RPON','mg/L','RPON',0.04_rk, minimum=0.0_rk,no_river_dilution=.true.) 
    call self%register_state_variable(self%id_LPON,'LPON','mg/L','LPON',0.04_rk, minimum=0.0_rk,no_river_dilution=.true.) 
    call self%register_state_variable(self%id_DON, 'DON', 'mg/L','DON', 0.06_rk, minimum=0.0_rk,no_river_dilution=.true.) 
    call self%register_state_variable(self%id_NH4, 'NH4', 'mg/L','NH4', 0.01_rk, minimum=0.0_rk,no_river_dilution=.true.) 
    call self%register_state_variable(self%id_NO3, 'NO3', 'mg/L','NO3', 0.05_rk, minimum=0.0_rk,no_river_dilution=.true.) 
    call self%register_state_variable(self%id_RPOP,'RPOP','mg/L','RPOP',0.004_rk,minimum=0.0_rk,no_river_dilution=.true.) 
    call self%register_state_variable(self%id_LPOP,'LPOP','mg/L','LPOP',0.004_rk,minimum=0.0_rk,no_river_dilution=.true.) 
    call self%register_state_variable(self%id_DOP, 'DOP', 'mg/L','DOP', 0.006_rk,minimum=0.0_rk,no_river_dilution=.true.) 
    call self%register_state_variable(self%id_PO4, 'PO4', 'mg/L','PO4', 0.01_rk, minimum=0.0_rk,no_river_dilution=.true.) 
    call self%register_state_variable(self%id_COD, 'COD', 'mg/L','COD', 0.0_rk,  minimum=0.0_rk,no_river_dilution=.true.) 
    call self%register_state_variable(self%id_DOX, 'DOX', 'mg/L','DOX', 10.0_rk, minimum=0.0_rk,no_river_dilution=.true.) 
     
    !sediment module 
    call self%register_state_variable(self%id_bPOC,'bPOC', 'g.m-2','Sediment POC concentration',0.0_rk, minimum=0.0_rk)
    call self%register_state_variable(self%id_bPON,'bPON', 'g.m-2','Sediment PON concentration',0.0_rk, minimum=0.0_rk)
    call self%register_state_variable(self%id_bPOP,'bPOP', 'g.m-2','Sediment POP concentration',0.0_rk, minimum=0.0_rk)

    !----------------------------------------------------------------------------------------------------
    !model parameters
    !----------------------------------------------------------------------------------------------------
    !default values
    self%iKe=0; self%Ke0=0.26; self%KeC=0.017; self%KeS=0.07; self%tss2c=6.0; self%alpha=(/10.0,10.0,3.15/)
    self%GPM=(/3.0, 3.0, 2.5/);    self%TGP=(/15.0, 22.0, 27.0/); self%PRR=(/0.215, 0.215, 0.01/); 
    self%MTB=(/0.01, 0.01, 0.04/); self%TMT=(/20.0, 20.0, 20.0/); self%KTMT=(/0.0322, 0.0322, 0.0322/); 
    self%WSPBS=(/0.35, 0.10, 0.00/);
    self%KTGP=reshape((/0.004, 0.008, 0.005, 0.006, 0.010, 0.004/),(/3,2/)); self%WSPOM=(/1.00, 1.00/); self%WSSED=1.00;
    self%FCP=reshape((/0.35, 0.30, 0.20, 0.55, 0.50, 0.50, 0.10, 0.20, 0.30/),(/3,3/)); self%FNP=(/0.35, 0.50, 0.10, 0.05/);
    self%FPP=(/0.10, 0.20, 0.50, 0.20/); self%FCM=(/0.10, 0.10, 0.10/); 
    self%FNM=reshape((/0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0/),(/3,4/)); 
    self%FPM=reshape((/0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0/),(/3,4/));
    self%Nit=0.07;     self%TNit=27.0;  self%KTNit=(/0.0045, 0.0045/);  self%KhDOn=1.0; 
    self%KhNH4n=1.0; self%KhDOox=0.5; self%KhNO3dn=0.1;
    self%KC0=(/0.005, 0.075, 0.20/); self%KN0=(/0.005, 0.075, 0.015/); self%KP0=(/0.005, 0.075, 0.010/); 
    self%KCalg=(/0.0, 0.0, 0.0/);    self%KNalg=(/0.0, 0.0, 0.0/);     self%KPalg=(/0.0, 0.0, 0.0/); 
    self%TRM=(/20.0, 20.0, 20.0/);   self%KTRM=(/0.069, 0.069, 0.069/);
    self%KCD=1.0;  self%TRCOD=20.0;  self%KTRCOD=0.041;  self%KhCOD=1.5;
    self%KhN=(/0.01, 0.01, 0.01/);      self%KhP=(/0.001, 0.001, 0.001/); self%KhSal=(/1e6, 1e6, 0.1/); 
    self%c2chl=(/0.059, 0.059, 0.059/); self%n2c=(/0.167, 0.167, 0.167/); self%p2c=(/0.02, 0.02, 0.02/); 
    self%KhDO=(/0.5, 0.5, 0.5/);        self%PBmin=(/0.0, 0.0, 0.0/);
    self%o2c=2.67;  self%o2n=4.33;  self%dn2c=0.933;  self%an2c=0.5;  self%KPO4p=0.0;  self%WRea=0.0
    self%bKC=0.01; self%bKN=0.01; self%bKP=0.01; self%bKTC=0.005; self%bKTN=0.005; self%bKTP=0.005;  !sediment module

    !scalar parameter
    call self%get_parameter(self%iKe,'iKe','none','flag for computing light attenuation',default=self%iKe)
    call self%get_parameter(self%Ke0,'Ke0','1/m','backgroud light extinction coefficient',default=self%Ke0)
    call self%get_parameter(self%KeC,'KeC','TBD','Light attenu. due to chlorophyll',default=self%KeC)
    call self%get_parameter(self%KeS,'KeS','TBD','Light attenu. due to TSS',default=self%KeS)
    call self%get_parameter(self%tss2c,'tss2c','None','TSS to carbon ratio',default=self%tss2c)
    call self%get_parameter(self%WSSED,'WSSED','m.day-1','settling velocity of TSS',default=self%WSSED)
    call self%get_parameter(self%Nit,'Nit','day-1','nitrification rate',default=self%Nit)
    call self%get_parameter(self%TNit,'TNit','oC','optimal temp. for nitrification',default=self%TNit)
    call self%get_parameter(self%KhDOn,'KhDOn','mg/L','DO half saturation for nitrification',default=self%KhDOn)
    call self%get_parameter(self%KhNH4n,'KhNH4n','mg/L','NH4 half saturation for nitrification',default=self%KhNH4n)
    call self%get_parameter(self%KhDOox,'KhDOox','mg/L','DO half saturation for dentrification & DOC oxic respiration',default=self%KhDOox)
    call self%get_parameter(self%KhNO3dn,'KhNO3dn','mg/L','NO3 half saturation for denitrification',default=self%KhNO3dn)
    call self%get_parameter(self%KCD,'KCD','day-1','oxidation rate of COD at TRCOD',default=self%KCD)
    call self%get_parameter(self%TRCOD,'TRCOD','oC','reference temp. for COD oxidation',default=self%TRCOD)
    call self%get_parameter(self%KTRCOD,'KTRCOD','oC-1','temp. dependence for COD oxidation',default=self%KTRCOD)
    call self%get_parameter(self%KhCOD,'KhCOD','mg[O2]/L','COD half saturation for COD oxidation',default=self%KhCOD)
    call self%get_parameter(self%o2c,'o2c','none','oxygen to carbon ratio in respiration',default=self%o2c)
    call self%get_parameter(self%o2n,'o2n','none','oxygen to ammonium ratio (g[O2]/g[NH4])',default=self%o2n)
    call self%get_parameter(self%dn2c,'dn2c','none','mass of NO3 consumed per mass of DOC oxidized in denit. (g[N]/g[C])',default=self%dn2c)
    call self%get_parameter(self%an2c,'an2c','none','ratio of denit. rate to oxic DOC respiration rate',default=self%an2c)
    call self%get_parameter(self%KPO4p,'KPO4p','none','coefficient relating PO4 sorption to TSS',default=self%KPO4p)
    call self%get_parameter(self%WRea,'WRea','day-1','baseline wind-induced reaeration coefficient for DO',default=self%WRea)

    !sediment
    call self%get_parameter(self%bKC, 'bKC', 'day-1','decay rate of sediment POC',default=self%bKC)
    call self%get_parameter(self%bKN, 'bKN', 'day-1','decay rate of sediment PON',default=self%bKN)
    call self%get_parameter(self%bKP, 'bKP', 'day-1','decay rate of sediment POP',default=self%bKP)
    call self%get_parameter(self%bKTC,'bKTC','oC-1', 'temp. dependence of sediment POC decay rate',default=self%bKTC)
    call self%get_parameter(self%bKTN,'bKTN','oC-1', 'temp. dependence of sediment PON decay rate',default=self%bKTN)
    call self%get_parameter(self%bKTP,'bKTP','oC-1', 'temp. dependence of sediment POP decay rate',default=self%bKTP)

    do i=1,3 
      !1D parameter array
      write(a,'(i1)') i
      if(i<=2) call self%get_parameter(self%WSPOM(i),'WSPOM_'//a,'m.day-1','settling velocity of RPOM & LPOM: '//a,default=self%WSPOM(i))
      if(i<=2) call self%get_parameter(self%KTNit(i),'KTNit_'//a,'temp. dependence for nitrification: '//a,default=self%KTNit(i))
      call self%get_parameter(self%alpha(i),'alpha_'//a,'none', 'init. slope of P-I curve: '//a,default=self%alpha(i))
      call self%get_parameter(self%GPM(i),  'GPM_'//a,  'day-1','PB growth rate: '//a,default=self%GPM(i))
      call self%get_parameter(self%TGP(i),  'TGP_'//a,  'oC',   'optimal temp. for PB growth: '//a,default=self%TGP(i))
      call self%get_parameter(self%PRR(i),  'PRR_'//a,  'day-1','predation rate: '//a,default=self%PRR(i))
      call self%get_parameter(self%MTB(i),  'MTB_'//a,  'day-1','PB metabolism rate: '//a,default=self%MTB(i))
      call self%get_parameter(self%TMT(i),  'TMT_'//a,  'oC','reference temp. for PB metabolism:  '//a,default=self%TMT(i))
      call self%get_parameter(self%KTMT(i), 'KTMT_'//a, 'oC-1','temp. dependence for PB metabolisim: '//a,default=self%KTMT(i))
      call self%get_parameter(self%WSPBS(i),'WSPBS_'//a,'m.day-1','settling velocity of PB: '//a,default=self%WSPBS(i))
      call self%get_parameter(self%FCM(i),'FCM_'//a,'none','fraction of PB metabolism carbon to (RPOC,LPOC,DOC): '//a,default=self%FCM(i))
      call self%get_parameter(self%KC0(i),'KC0_'//a,'day-1','minimum decay rate of RPOC,LPOC,DOC: '//a,default=self%KC0(i))
      call self%get_parameter(self%KN0(i),'KN0_'//a,'day-1','minimum decay rate of RPON,LPON,DON: '//a,default=self%KN0(i))
      call self%get_parameter(self%KP0(i),'KP0_'//a,'day-1','minimum decay rate of RPOP,LPOP,DOP: '//a,default=self%KP0(i))
      call self%get_parameter(self%KCalg(i),'KCalg_'//a,'day-1.m3.g[C]-1','algae effect on RPOC,LPOC,DOC decay: '//a,default=self%KCalg(i))
      call self%get_parameter(self%KNalg(i),'KNalg_'//a,'day-1.m3.g[C]-1','algae effect on RPON,LPON,DON decay: '//a,default=self%KNalg(i))
      call self%get_parameter(self%KPalg(i),'KPalg_'//a,'day-1.m3.g[C]-1','algae effect on RPOP,LPOP,DOP decay: '//a,default=self%KPalg(i))
      call self%get_parameter(self%TRM(i),'TRM_'//a,'oC','reference temp. for (RPOM,LPOM,DOM) decay: '//a,default=self%TRM(i))
      call self%get_parameter(self%KTRM(i),'KTRM_'//a,'oC-1','temp. dependence for (RPOM,LPOM,DOM) decay: '//a,default=self%KTRM(i))
      call self%get_parameter(self%KhN(i),'KhN_'//a,'mg/L','nitrogen half saturation: '//a,default=self%KhN(i))
      call self%get_parameter(self%KhP(i),'KhP_'//a,'mg/L','phosphorus half saturation: '//a,default=self%KhP(i))
      call self%get_parameter(self%KhSal(i),'KhSal_'//a,'PSU','salinity when PB growth is halved (PSU); (1e6: no salinity stress) '//a,default=self%KhSal(i))
      call self%get_parameter(self%c2chl(i),'c2chl_'//a,'g[C]/mg[Chl]','carbon to chlorophyll ratio: '//a,default=self%c2chl(i))
      call self%get_parameter(self%n2c(i),'n2c_'//a,'none','nitrogen to carbon ratio for phytoplankton:'//a,default=self%n2c(i))
      call self%get_parameter(self%p2c(i),'p2c_'//a,'none','phosphorus to carbon ratio for phytoplankton:'//a,default=self%p2c(i))
      call self%get_parameter(self%KhDO(i),'KhDO_'//a,'mg/L','DO half saturation for PB DOC excretio:'//a,default=self%KhDO(i))
      call self%get_parameter(self%PBmin(i),'PBmin_'//a,'mg[C]/L','minimum PB concentration :'//a,default=self%PBmin(i))

      !2D parameter array (exception for FNP,FPP,FCM)
      do j=1,4
        write(b,'(i1)') j
        if(i==1) then ! still 1D array
          call self%get_parameter(self%FNP(j),'FNP_'//b,'none','fraction of PB nitrogen to (RPON,LPON,DON,NH4): '//b,default=self%FNP(j))
          call self%get_parameter(self%FPP(j),'FPP_'//b,'none','fraction of PB phosphorus to (RPOP,LPOP,DOP,PO4): '//b,default=self%FPP(j))
        endif
        if(j<=2) call self%get_parameter(self%KTGP(i,j),'KTGP_'//a//b,'oC-1','temp. dependence for PB growth: '//a//b,default=self%KTGP(i,j))
        if(j<=3) call self%get_parameter(self%FCP(i,j), 'FCP_'//a//b,'none','fraction of PB carbon to (RPOC,LPOC,DOC): '//a//b,default=self%FCP(i,j))
        call self%get_parameter(self%FNM(i,j), 'FNM_'//a//b,'none','fraction of PB N. to (RPON,LPON,DON,NH4): '//a//b,default=self%FNM(i,j))
        call self%get_parameter(self%FPM(i,j), 'FPM_'//a//b,'none','fraction of PB P. to (RPOP,LPOP,DOP,PO4): '//a//b,default=self%FPM(i,j))
      enddo
    enddo

    !----------------------------------------------------------------------------------------------------
    !diagnostic variables
    !----------------------------------------------------------------------------------------------------
    call self%register_diagnostic_variable(self%id_dPAR,'dPAR','W m-2','photosynthetic_radiative_flux', missing_value=0.0_rk, &
       & standard_variable=standard_variables%downwelling_photosynthetic_radiative_flux, source=source_do_column)

    !----------------------------------------------------------------------------------------------------
    !enviromental dependencies
    !----------------------------------------------------------------------------------------------------
    call self%register_dependency(self%id_temp,  standard_variables%temperature)
    call self%register_dependency(self%id_salt,  standard_variables%practical_salinity)
    call self%register_dependency(self%id_zr,    standard_variables%depth)
    call self%register_dependency(self%id_dz,    standard_variables%cell_thickness)
    call self%register_dependency(self%id_PAR,   standard_variables%downwelling_photosynthetic_radiative_flux) 
    call self%register_dependency(self%id_Light0,standard_variables%surface_downwelling_shortwave_flux)
    call self%register_dependency(self%id_wspd,  standard_variables%wind_speed)
    call self%register_dependency(self%id_dt,    type_global_standard_variable(name='time_step_of_host_model',units='second'))
    !call self%register_dependency(self%id_GPM_1, type_horizontal_standard_variable(name='GPM_1',units='day-1'))

  end subroutine initialize

  subroutine do(self, _ARGUMENTS_DO_)
    class (type_vims_icm), intent(in) :: self
     _DECLARE_ARGUMENTS_DO_

    real(rk),parameter :: s2d=86400.0_rk
    integer,parameter :: ntrs=17
    integer,parameter :: iPB1=1,iPB2=2,iPB3=3,iRPOC=4,iLPOC=5,iDOC=6,iRPON=7,iLPON=8,iDON=9, &
                       & iNH4=10,iNO3=11,iRPOP=12,iLPOP=13,iDOP=14,iPO4=15,iCOD=16,iDOX=17

    integer :: i,m
    real(rk) :: PBS(3),RPOC,LPOC,DOC,RPON,LPON,DON,NH4,NO3,RPOP,LPOP,DOP,PO4,COD,DOX
    real(rk) :: temp,salt,zr,dz,PAR,TSS,DIN,PO4d,PO4p,rIK,fT,fN,fP,fST,fR,rKTM
    real(rk) :: dwqc(ntrs),rat,dt,xT,APB,mKhN,mKhP,rKHR,rKCOD,rDenit,rNit !,GPM_1
    real(rk),dimension(3) :: GP,MT,PR,rKC,rKN,rKP,fPN

    ! Enter spatial loops (if any)
    _LOOP_BEGIN_
      !--------------------------------------------------------------------
      !get the values of local variables 
      !--------------------------------------------------------------------
      _GET_(self%id_PB1, PBS(1))
      _GET_(self%id_PB2, PBS(2))
      _GET_(self%id_PB3, PBS(3))
      _GET_(self%id_RPOC,RPOC)
      _GET_(self%id_LPOC,LPOC)
      _GET_(self%id_DOC, DOC)
      _GET_(self%id_RPON,RPON)
      _GET_(self%id_LPON,LPON)
      _GET_(self%id_DON, DON)
      _GET_(self%id_NH4, NH4)
      _GET_(self%id_NO3, NO3)
      _GET_(self%id_RPOP,RPOP)
      _GET_(self%id_LPOP,LPOP)
      _GET_(self%id_DOP, DOP)
      _GET_(self%id_PO4, PO4)
      _GET_(self%id_COD, COD)
      _GET_(self%id_DOX, DOX)

      !get other values 
      _GET_(self%id_temp,temp)
      _GET_(self%id_salt,salt)
      _GET_(self%id_zr,  zr)
      _GET_(self%id_dz,  dz)
      _GET_(self%id_PAR, PAR)
      _GET_GLOBAL_(self%id_dt,  dt)
      !_GET_HORIZONTAL_(self%id_GPM_1,GPM_1)

      !pre-computation
      if(self%iKe==0) TSS=self%tss2c*(RPOC+LPOC) 
      !if(self%iKe==1) TSS from SED3D or source !todo: fix this
      DIN=NH4+NO3
      rat=1.0/(1.0+self%KPO4p*TSS); PO4d=rat*PO4; PO4p=(1.0-rat)*PO4
      APB=sum(PBS); mKhN=sum(self%KhN)/3.0; mKhP=sum(self%KhP)/3.0
      !-----------------------------------------------------
      !compute phytoplankton growth rate
      !-----------------------------------------------------
      do i=1,3
        fST=1.0; xT=temp-self%TGP(i)

        !limitation factors: T/N/P/Sal
        if(xT<=0) then
          fT=exp(-self%KTGP(i,1)*xT**2.d0) !temp.
        else
          fT=exp(-self%KTGP(i,2)*xT**2.d0) !temp.
        endif
        fN=DIN/(DIN+self%KhN(i))   !nitrogen
        fP=PO4d/(PO4d+self%KhP(i)) !phosphorus
        if(self%KhSal(i)<100.d0) fST=(self%KhSal(i)**2.d0)/((self%KhSal(i)**2.d0)+salt*salt) !salinity

        !Light factor
        rIK=(1.d3*self%c2chl(i))*fT*self%GPM(i)/self%alpha(i)
        fR=PAR/sqrt(PAR*PAR+rIK*rIK+1.e-12)

        !growth  
        GP(i)=self%GPM(i)*fT*fST*fR*min(fN,fP)*PBS(i)

        !metabolism, predation
        MT(i)=self%MTB(i)*exp(self%KTMT(i)*(temp-self%TMT(i)))*PBS(i)
        PR(i)=self%PRR(i)*exp(self%KTMT(i)*(temp-self%TMT(i)))*PBS(i)
      
        !decay rates of organic matter
        rKTM=exp(self%KTRM(i)*(temp-self%TRM(i)))
        rKC(i)=(self%KC0(i)+self%KCalg(i)*APB)*rKTM
        rKN(i)=(self%KN0(i)+self%KNalg(i)*APB*mKhN/(mKhN+DIN))*rKTM
        rKP(i)=(self%KP0(i)+self%KPalg(i)*APB*mKhP/(mKhP+PO4d))*rKTM 

        !nitrogen preference
        if(DIN>0.d0) fPN(i)=(NH4/(self%KhN(i)+NO3))*(NO3/(self%KhN(i)+NH4)+self%KhN(i)/(DIN+1.d-6))
      enddo !i

      !respiration, denitrification, decay of COD, nitrification
      xT=temp-self%TNit
      rKHR=rKC(3)*DOX/(self%KhDOox+DOX)
      rKCOD=(DOX/(self%KhCOD+DOX))*self%KCD*exp(self%KTRCOD*(temp-self%TRCOD))
      rDenit=self%an2c*rKC(3)*self%KhDOox*NO3/(self%KhDOox+DOX)/(self%KhNO3dn+NO3)
      if(xT<=0) then
        rNit=(DOX*self%Nit*self%KhNH4n/((self%KhNH4n+NH4)*(self%KhDOn+DOX)))*exp(-self%KTNit(1)*xT*xT) 
      else
        rNit=(DOX*self%Nit*self%KhNH4n/((self%KhNH4n+NH4)*(self%KhDOn+DOX)))*exp(-self%KTNit(1)*xT*xT) 
      endif

      !----------------------------------------------------------------------------------
      !changes of state variables at each layer
      !--------------------------------------------------------------------------------
      dwqc=0.0  !mg.L-1.day-1
      !PB1, PB2, PB3
      dwqc(iPB1)=GP(1)-MT(1)-PR(1) !growth, metabolism, predation
      dwqc(iPB2)=GP(2)-MT(2)-PR(2) !growth, metabolism, predation
      dwqc(iPB3)=GP(3)-MT(3)-PR(3) !growth, metabolism, predation

      !RPOC, LPOC, DOC
      dwqc(iRPOC)=-rKC(1)*RPOC !dissolution
      dwqc(iLPOC)=-rKC(2)*LPOC !dissolution
      dwqc(iDOC) = rKC(1)*RPOC+rKC(2)*LPOC-(rKHR+rDenit)*DOC !dissolution, respiration, denitrificat
      do m=1,3
        dwqc(iRPOC)=dwqc(iRPOC)+self%FCP(m,1)*PR(m) !predation
        dwqc(iLPOC)=dwqc(iLPOC)+self%FCP(m,2)*PR(m) !predation
        dwqc(iDOC) =dwqc(iDOC) +self%FCP(m,3)*PR(m)+(self%FCM(m)+(1.0-self%FCM(m))*self%KhDO(m)/(DOX+self%KhDO(m)))*MT(m) !predation,metabolism
      enddo

      !RPON, LPON, DON, NH4, NO3
      dwqc(iRPON)=-rKN(1)*RPON !dissolution
      dwqc(iLPON)=-rKN(2)*LPON !dissolution
      dwqc(iDON) = rKN(1)*RPON+rKN(2)*LPON-rKN(3)*DON !dissolution, mineralization
      dwqc(iNH4) = rKN(3)*DON-rNit*NH4  !mineralization, nitrification
      dwqc(iNO3) = rNit*NH4-self%dn2c*rDenit*DOC  !nitrification, denitrification
      do m=1,3
        dwqc(iRPON)=dwqc(iRPON)+self%n2c(m)*(self%FNP(1)*PR(m)+self%FNM(m,1)*MT(m)) !predation, metabolism 
        dwqc(iLPON)=dwqc(iLPON)+self%n2c(m)*(self%FNP(2)*PR(m)+self%FNM(m,2)*MT(m)) !predation, metabolism 
        dwqc(iDON) =dwqc(iDON) +self%n2c(m)*(self%FNP(3)*PR(m)+self%FNM(m,3)*MT(m)) !predation, metabolism  
        dwqc(iNH4) =dwqc(iNH4) +self%n2c(m)*(self%FNP(4)*PR(m)+self%FNM(m,4)*MT(m)-fPN(m)*GP(m)) !predation, metabolism, growth
        dwqc(iNO3) =dwqc(iNO3) -self%n2c(m)*(1.0-fPN(m))*GP(m) !growth
      enddo

      !RPOP, LPOP, DOP, PO4
      dwqc(iRPOP)=-rKP(1)*RPOP !dissolution
      dwqc(iLPOP)=-rKP(2)*LPOP !dissolution
      dwqc(iDOP) = rKP(1)*RPOP+rKP(2)*LPOP-rKP(3)*DOP !dissolution, mineralization
      dwqc(iPO4) = rKP(3)*DOP !mineralization
      do m=1,3
        dwqc(iRPOP)=dwqc(iRPOP)+self%p2c(m)*(self%FPP(1)*PR(m)+self%FPM(m,1)*MT(m)) !predation, metabolism 
        dwqc(iLPOP)=dwqc(iLPOP)+self%p2c(m)*(self%FPP(2)*PR(m)+self%FPM(m,2)*MT(m)) !predation, metabolism 
        dwqc(iDOP) =dwqc(iDOP) +self%p2c(m)*(self%FPP(3)*PR(m)+self%FPM(m,3)*MT(m)) !predation, metabolism  
        dwqc(iPO4) =dwqc(iPO4) +self%p2c(m)*(self%FPP(4)*PR(m)+self%FPM(m,4)*MT(m)-GP(m)) !predation, metabolism, growth
      enddo

      !COD 
      dwqc(iCOD)=-rKCOD*COD !oxidation

      !DO
      dwqc(iDOX)=-self%o2n*rNit*NH4-self%o2c*rKHR*DOC-rKCOD*COD !nitrification, respiration, COD oxidiation
      do m=1,3
        dwqc(iDOX)=dwqc(iDOX)+self%o2c*((1.3-0.3*fPN(m))*GP(m)-((1.0-self%FCM(m))*DOX/(DOX+self%KhDO(m)))*MT(m)) !growth, metabolism
      enddo

      !-----------------------------------------------------------------
      ! Set temporal derivatives
      !-----------------------------------------------------------------
      dwqc=dwqc/s2d
      _ADD_SOURCE_(self%id_PB1,  dwqc(iPB1))
      _ADD_SOURCE_(self%id_PB2,  dwqc(iPB2))
      _ADD_SOURCE_(self%id_PB3,  dwqc(iPB3))
      _ADD_SOURCE_(self%id_RPOC, dwqc(iRPOC))
      _ADD_SOURCE_(self%id_LPOC, dwqc(iLPOC))
      _ADD_SOURCE_(self%id_DOC,  dwqc(iDOC))
      _ADD_SOURCE_(self%id_RPON, dwqc(iRPON))
      _ADD_SOURCE_(self%id_LPON, dwqc(iLPON))
      _ADD_SOURCE_(self%id_DON,  dwqc(iDON))
      _ADD_SOURCE_(self%id_NH4,  dwqc(iNH4))
      _ADD_SOURCE_(self%id_NO3,  dwqc(iNO3))
      _ADD_SOURCE_(self%id_RPOP, dwqc(iRPOP))
      _ADD_SOURCE_(self%id_LPOP, dwqc(iLPOP))
      _ADD_SOURCE_(self%id_DOP,  dwqc(iDOP))
      _ADD_SOURCE_(self%id_PO4,  dwqc(iPO4))
      _ADD_SOURCE_(self%id_COD,  dwqc(iCOD))
      _ADD_SOURCE_(self%id_DOX,  dwqc(iDOX))

    _LOOP_END_
  end subroutine do

  subroutine get_vertical_movement(self, _ARGUMENTS_GET_VERTICAL_MOVEMENT_)
    class (type_vims_icm), intent(in) :: self
    _DECLARE_ARGUMENTS_GET_VERTICAL_MOVEMENT_

    real(rk),parameter :: s2d=86400.0_rk
    !integer,parameter :: ntrs=17
    !integer,parameter :: iPB1=1,iPB2=2,iPB3=3,iRPOC=4,iLPOC=5,iDOC=6,iRPON=7,iLPON=8,iDON=9, &
    !                   & iNH4=10,iNO3=11,iRPOP=12,iLPOP=13,iDOP=14,iPO4=15,iCOD=16,iDOX=17

    real(rk) :: fp,TSS,RPOC,LPOC

    _LOOP_BEGIN_
      !pre-compute
      _GET_(self%id_RPOC,RPOC)
      _GET_(self%id_LPOC,LPOC)
      if(self%iKe==0) TSS=self%tss2c*(RPOC+LPOC)
      !if(self%iKe==1) TSS from SED3D or source !todo: fix this
      fp=self%KPO4p*TSS/(1.0+self%KPO4p*TSS)

      !add sink velocity
      _ADD_VERTICAL_VELOCITY_(self%id_PB1,  -self%WSPBS(1)/s2d)
      _ADD_VERTICAL_VELOCITY_(self%id_PB2,  -self%WSPBS(2)/s2d)
      _ADD_VERTICAL_VELOCITY_(self%id_PB3,  -self%WSPBS(3)/s2d)
      _ADD_VERTICAL_VELOCITY_(self%id_RPOC, -self%WSPOM(1)/s2d)
      _ADD_VERTICAL_VELOCITY_(self%id_LPOC, -self%WSPOM(2)/s2d)
      _ADD_VERTICAL_VELOCITY_(self%id_DOC,  0.d0)
      _ADD_VERTICAL_VELOCITY_(self%id_RPON, -self%WSPOM(1)/s2d)
      _ADD_VERTICAL_VELOCITY_(self%id_LPON, -self%WSPOM(2)/s2d)
      _ADD_VERTICAL_VELOCITY_(self%id_DON,  0.d0)
      _ADD_VERTICAL_VELOCITY_(self%id_NH4,  0.d0)
      _ADD_VERTICAL_VELOCITY_(self%id_NO3,  0.d0)
      _ADD_VERTICAL_VELOCITY_(self%id_RPOP, -self%WSPOM(1)/s2d)
      _ADD_VERTICAL_VELOCITY_(self%id_LPOP, -self%WSPOM(2)/s2d)
      _ADD_VERTICAL_VELOCITY_(self%id_DOP,  0.d0)
      _ADD_VERTICAL_VELOCITY_(self%id_PO4,  -self%WSSED*fp/s2d)
      _ADD_VERTICAL_VELOCITY_(self%id_COD,  0.d0)
      _ADD_VERTICAL_VELOCITY_(self%id_DOX,  0.d0)

    _LOOP_END_
  end subroutine get_vertical_movement

  subroutine do_surface(self, _ARGUMENTS_DO_SURFACE_)
    class (type_vims_icm), intent(in) :: self
    _DECLARE_ARGUMENTS_DO_SURFACE_

    real(rk),parameter :: s2d=86400.0_rk
    real(rk) :: temp,salt,DOX,wspd,zr,dz,DOsat,rKa,FLUX_DOX

    _SURFACE_LOOP_BEGIN_
      !get values of variables
      _GET_(self%id_temp,temp)
      _GET_(self%id_salt,salt)
      _GET_(self%id_DOX, DOX)
      _GET_(self%id_zr,  zr)
      _GET_(self%id_dz,  dz)
      _GET_SURFACE_(self%id_wspd,wspd)

      !compute saturated DO, air-sea flux of DO (Genet et al. 1974; Carl Cerco,2002,201?)
      DOsat=14.5532-0.38217*temp+5.4258e-3*temp*temp-salt*(1.665e-4-5.866e-6*temp+9.796e-8*temp*temp)/1.80655
      rKa=self%WRea+0.157*(0.54+0.0233*temp-0.002*salt)*wspd**1.5
      FLUX_DOX=rKa*(DOsat-DOX)

      !add surface fluxes
      _ADD_SURFACE_FLUX_(self%id_PB1,  0.d0)
      _ADD_SURFACE_FLUX_(self%id_PB2,  0.d0)
      _ADD_SURFACE_FLUX_(self%id_PB3,  0.d0)
      _ADD_SURFACE_FLUX_(self%id_RPOC, 0.d0)
      _ADD_SURFACE_FLUX_(self%id_LPOC, 0.d0)
      _ADD_SURFACE_FLUX_(self%id_DOC,  0.d0)
      _ADD_SURFACE_FLUX_(self%id_RPON, 0.d0)
      _ADD_SURFACE_FLUX_(self%id_LPON, 0.d0)
      _ADD_SURFACE_FLUX_(self%id_DON,  0.d0)
      _ADD_SURFACE_FLUX_(self%id_NH4,  0.d0)
      _ADD_SURFACE_FLUX_(self%id_NO3,  0.d0)
      _ADD_SURFACE_FLUX_(self%id_RPOP, 0.d0)
      _ADD_SURFACE_FLUX_(self%id_LPOP, 0.d0)
      _ADD_SURFACE_FLUX_(self%id_DOP,  0.d0)
      _ADD_SURFACE_FLUX_(self%id_PO4,  0.d0)
      _ADD_SURFACE_FLUX_(self%id_COD,  0.d0)
      _ADD_SURFACE_FLUX_(self%id_DOX,  FLUX_DOX/s2d)

    _SURFACE_LOOP_END_
  end subroutine do_surface

  subroutine do_bottom(self, _ARGUMENTS_DO_BOTTOM_)
    class (type_vims_icm),intent(in) :: self
    _DECLARE_ARGUMENTS_DO_BOTTOM_

    real(rk),parameter :: s2d=86400.0_rk
    integer :: m
    real(rk) :: PBS(3),RPOC,LPOC,RPON,LPON,RPOP,LPOP,PO4,bPOC,bPON,bPOP
    real(rk) :: temp,TSS,fp,dPOC,dPON,dPOP,FLUX_CO2,FLUX_NH4,FLUX_PO4,SOD

    _BOTTOM_LOOP_BEGIN_
      !get values of model variables 
      _GET_(self%id_temp,temp)
      _GET_(self%id_PB1, PBS(1))
      _GET_(self%id_PB2, PBS(2))
      _GET_(self%id_PB3, PBS(3))
      _GET_(self%id_RPOC,RPOC)
      _GET_(self%id_LPOC,LPOC)
      _GET_(self%id_RPON,RPON)
      _GET_(self%id_LPON,LPON)
      _GET_(self%id_RPOP,RPOP)
      _GET_(self%id_LPOP,LPOP)
      _GET_(self%id_PO4, PO4)
      _GET_BOTTOM_(self%id_bPOC, bPOC) 
      _GET_BOTTOM_(self%id_bPON, bPON) 
      _GET_BOTTOM_(self%id_bPOP, bPOP) 

      !pre-compute
      if(self%iKe==0) TSS=self%tss2c*(RPOC+LPOC)
      !if(self%iKe==1) TSS from SED3D or source !todo: fix this
      fp=self%KPO4p*TSS/(1.0+self%KPO4p*TSS)

      !compute decay of sediment POM
      FLUX_CO2=bPOC*self%bKC*exp(self%bKTC*(temp-20.d0)); SOD=FLUX_CO2*self%o2c
      FLUX_NH4=bPON*self%bKN*exp(self%bKTN*(temp-20.d0))
      FLUX_PO4=bPOP*self%bKP*exp(self%bKTP*(temp-20.d0))

      !compute changes of sediment POM
      dPOC=self%WSPOM(1)*RPOC+self%WSPOM(2)*LPOC-FLUX_CO2
      dPON=self%WSPOM(1)*RPON+self%WSPOM(2)*LPON-FLUX_NH4
      dPOP=self%WSPOM(1)*RPOP+self%WSPOM(2)*LPOP+fp*self%WSSED*PO4-FLUX_PO4
      do m=1,3
        dPOC=dPOC+self%WSPBS(m)*PBS(m)
        dPON=dPON+self%n2c(m)*self%WSPBS(m)*PBS(m)
        dPOP=dPOP+self%p2c(m)*self%WSPBS(m)*PBS(m)
      enddo

      !update sediment POM
      _ADD_BOTTOM_SOURCE_(self%id_bPOC, dPOC/s2d)
      _ADD_BOTTOM_SOURCE_(self%id_bPON, dPON/s2d)
      _ADD_BOTTOM_SOURCE_(self%id_bPOP, dPOP/s2d)

      !add sediment fluxes
      _ADD_BOTTOM_FLUX_(self%id_NH4, FLUX_NH4/s2d)
      _ADD_BOTTOM_FLUX_(self%id_PO4, FLUX_PO4/s2d)
      _ADD_BOTTOM_FLUX_(self%id_DOX, -SOD/s2d)

    _BOTTOM_LOOP_END_
  end subroutine do_bottom

  subroutine do_column(self, _ARGUMENTS_DO_COLUMN_)
    class (type_vims_icm), intent(in) :: self
     _DECLARE_ARGUMENTS_DO_COLUMN_
     
     real(rk),parameter :: Rrat=0.18659_rk !0.47*0.397 convert srad(W/m2) to PAR (E.m-2.day-1)
     real(rk) :: zr,dz,Light0,PAR,chl,TSS,RPOC,LPOC,PBS(3)
     real(rk) :: Ke

     _GET_SURFACE_(self%id_Light0,Light0) !surface short-wave radiation (W/m2)

     _DOWNWARD_LOOP_BEGIN_
        !get value at each layer
        _GET_(self%id_dz,dz)     ! Layer height (m)
        _GET_(self%id_zr,zr)     ! layer depth (m)
        _GET_(self%id_PB1,PBS(1)) 
        _GET_(self%id_PB2,PBS(2)) 
        _GET_(self%id_PB3,PBS(3)) 
        _GET_(self%id_RPOC,RPOC) 
        _GET_(self%id_LPOC,LPOC) 

        !pre-compute chl and TSS
        chl=max(sum(PBS/self%c2chl),0.d0)
        if(self%iKe==0) TSS=self%tss2c*(RPOC+LPOC)
        !if(self%iKe==1) TSS from standard variables: todo: add this option
       
        !compute PAR at layer center and layer bottom
        Ke=self%Ke0+self%KeC*chl+self%KeS*TSS
        PAR=Rrat*Light0*exp(-Ke*dz/2.0)
        Light0=Light0*exp(-Ke*dz)

        _SET_DIAGNOSTIC_(self%id_dPAR,PAR) ! Photosynthetically active radiation at layer centre
     _DOWNWARD_LOOP_END_
  end subroutine do_column

end module vims_icm
