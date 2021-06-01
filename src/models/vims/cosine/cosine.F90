#include "fabm_driver.h"
!------------------------------------------------------------------------------------
!CoSiNE 13: This is a placeholder for the cosine model only !!
!------------------------------------------------------------------------------------
! CoSiNE stands for Carbon, Silicate, Nitrogen Ecosystem, which was originally
! developed by Prof. Fei Chai (U. of Maine) for modeling the ocean biogeochemical
! processes for the equatorial Pacific and the Pacific Ocean (Chai, Dugdale et al.
! 2002, Chai, Jiang et al. 2003, Chai, Jiang et al. 2007). 
!
! Original author(s): Fei Chai (U Maine)
!
! @author  => add authors
! @copyright  => add VIMS etc ...
! @license => speak to Fei Chai, or is there a license for Cosine already?

!------------------------------------------------------------------------------------
!In CoSiNE model, there are 13 state variables 
!------------------------------------------------------------------------------------
! Nitrate               NO3   1   mmol/m3
! Silicate              SiO4  2   mmol/m3
! Ammonium              NH4   3   mmol/m3
! Small Phytoplankton   S1    4   mmol/m3
! Diatom                S2    5   mmol/m3
! Microzooplankton      Z1    6   mmol/m3
! Mesozooplankton       Z2    7   mmol/m3
! Detritus Nitrogen     DN    8   mmol/m3
! Detritus Silicon      DSi   9   mmol/m3
! Phosphate             PO4   10  mmol/m3
! Dissolved Oxygen      DOX   11  mmol m-3
! Dioxide Carbon        CO2   12  mmol m-3
! Alkalinity            ALK   13  meq m-3

module vims_cosine

   use fabm_types

   implicit none

   private

   type, extends(type_base_model), public :: type_vims_cosine
      !variable identifiers
      type (type_state_variable_id) :: id_NO3, id_SiO4, id_NH4
      type (type_state_variable_id) :: id_S1,  id_S2,   id_Z1,  id_Z2
      type (type_state_variable_id) :: id_DN,  id_DSi,  id_PO4
      type (type_state_variable_id) :: id_DOX, id_CO2,  id_ALK

      !dependence
      type (type_dependency_id) :: id_temp,id_salt,id_PAR,id_dep,id_zr
     
      !dianostic
      type (type_diagnostic_variable_id) :: id_PPR

      !model parameters
      integer  :: idapt,ico2s,iz2graze
      real(rk) :: gmaxs1,gmaxs2,pis1,pis2,kno3s1,knh4s1,kpo4s1,kco2s1,kno3s2,knh4s2
      real(rk) :: kpo4s2,kco2s2,ksio4s2,kns1,kns2,alpha1,alpha2,ak1,ak2,ak3,beta,gammas1,gammas2
      real(rk) :: beta1,beta2,kgz1,kgz2,rho1,rho2,rho3,gamma1,gamma2,gammaz,kex1,kex2,wss2,wsdn,wsdsi
      real(rk) :: si2n,p2n,o2no,o2nh,c2n,kox,kmdn1,kmdn2,kmdsi1,kmdsi2,gamman,TR,pco2a
      real(rk) :: alpha_corr,zeptic,ndelay,rdelay

   contains
      procedure :: initialize
      procedure :: do
      procedure :: do_ppdd
   end type

contains

   subroutine initialize(self, configunit)
      class (type_vims_cosine), intent(inout), target :: self
      integer,                intent(in)              :: configunit

      real(rk), parameter :: d_per_s = 1.0_rk / 86400.0_rk
      
      !--------------------------------------------------------
      !Register state variables, this must follow SCHISM-COSINE 
      !order, as SCHISM relies on this ordering
      !--------------------------------------------------------
      call self%register_state_variable(self%id_NO3, 'NO3', 'mmol m-3','Nitrate',            10.0_rk,minimum=0.0_rk,no_river_dilution=.true.)
      call self%register_state_variable(self%id_SiO4,'SiO4','mmol m-3','Silicate',           30.0_rk,minimum=0.0_rk,no_river_dilution=.true.)
      call self%register_state_variable(self%id_NH4, 'NH4', 'mmol m-3','Ammonium',           2.0_rk, minimum=0.0_rk,no_river_dilution=.true.)
      call self%register_state_variable(self%id_S1,  'S1',  'mmol m-3','Small Phytoplankton',0.5_rk, minimum=0.0_rk,no_river_dilution=.true.)
      call self%register_state_variable(self%id_S2,  'S2',  'mmol m-3','Diatom',             5.0_rk, minimum=0.0_rk,no_river_dilution=.true.)
      call self%register_state_variable(self%id_Z1,  'Z1',  'mmol m-3','Microzooplankton',   0.05_rk,minimum=0.0_rk,no_river_dilution=.true.)
      call self%register_state_variable(self%id_Z2,  'Z2',  'mmol m-3','Mesozooplankton',    0.5_rk, minimum=0.0_rk,no_river_dilution=.true.)
      call self%register_state_variable(self%id_DN,  'DN',  'mmol m-3','Detritus Nitrogen',  1.0_rk, minimum=0.0_rk,no_river_dilution=.true.)
      call self%register_state_variable(self%id_DSi, 'DSi', 'mmol m-3','Detritus Silicon',   2.0_rk, minimum=0.0_rk,no_river_dilution=.true.)
      call self%register_state_variable(self%id_PO4, 'PO4', 'mmol m-3','Phosphate',          3.0_rk, minimum=0.0_rk,no_river_dilution=.true.)
      call self%register_state_variable(self%id_DOX, 'DOX', 'mmol m-3','Dissolved Oxygen', 280.0_rk, minimum=0.0_rk,no_river_dilution=.true.)
      call self%register_state_variable(self%id_CO2, 'CO2', 'mmol m-3','Carbon Dioxide',  1950.0_rk, minimum=0.0_rk,no_river_dilution=.true.)
      call self%register_state_variable(self%id_ALK, 'ALK', 'meq m-3', 'Carbon Dioxide',  2100.0_rk, minimum=0.0_rk,no_river_dilution=.true.)

      !--------------------------------------------------------
      !Note: parameter values in our own derived type
      !1). all rates must be provided in values per day in the configuration file,
      !2). are converted here to values per second by specifying scale_factor=d_per_s.
      !--------------------------------------------------------

      !phytoplankton
      call self%get_parameter(self%gmaxs1,'gmaxs1','day-1','maximum growth rate of small phytoplankton',default=3.0_rk)
      call self%get_parameter(self%gmaxs2,'gmaxs2','day-1','maximum growth rate of diatom',default=2.0_rk)
      call self%get_parameter(self%pis1,'pis1','mmol-1 m3','ammonium inhibition rate for small phytoplankton',default=1.5_rk)
      call self%get_parameter(self%pis2,'pis2','mmol-1 m3','ammonium inhibition rate for diatom',default=1.5_rk)

      call self%get_parameter(self%kno3s1,'kno3s1','mmol m-3','half saturation of nitrate uptake by small phytoplankton',default=1.0_rk)
      call self%get_parameter(self%knh4s1,'knh4s1','mmol m-3','half saturation of ammonium uptake by small phytoplankton',default=0.15_rk)
      call self%get_parameter(self%kpo4s1,'kpo4s1','mmol m-3','half saturation of phosphate uptake by small phytoplankton',default=0.1_rk)
      call self%get_parameter(self%kco2s1,'kco2s1','mmol m-3','half saturation of CO2 uptake by small phytoplankton',default=50.0_rk)

      call self%get_parameter(self%kno3s2,'kno3s2','mmol m-3','half saturation of nitrate uptake by diatom',default=3.0_rk)
      call self%get_parameter(self%knh4s2,'knh4s2','mmol m-3','half saturation of ammonium uptake by diatom',default=0.45_rk)
      call self%get_parameter(self%kpo4s2,'kpo4s2','mmol m-3','half saturation of phosphate uptake by diatom',default=0.1_rk)
      call self%get_parameter(self%kco2s2,'kco2s2','mmol m-3','half saturation of CO2 uptake by diatom',default=50.0_rk)
      call self%get_parameter(self%kco2s2,'ksio4s2','mmol m-3','half saturation of silicon uptake by diatom',default=4.5_rk)

      call self%get_parameter(self%kns1,'kns1','none','nighttime NH4 update ratio for small phytoplankgon',default=0.0_rk)
      call self%get_parameter(self%kns2,'kns2','none','nighttime NH4 update ratio for diatom',default=0.0_rk)

      call self%get_parameter(self%alpha1,'alpha1','W-1 m2 day-1','initial slope of P-I curve for small phytoplantkon',default=0.1_rk)
      call self%get_parameter(self%alpha2,'alpha2','W-1 m2 day-1','initial slope of P-I curve for diatom',default=0.1_rk)
      call self%get_parameter(self%beta,'beta','W-1 m2 day-1','photo-inhibition constant',default=0.0_rk)
      call self%get_parameter(self%ak1,'ak1','m-1','light attenuation due to water',default=0.75_rk)
      call self%get_parameter(self%ak2,'ak2','mmol-1 m-1','light attenuation due to phytoplankton',default=0.03_rk)
      call self%get_parameter(self%ak3,'ak3','g-1 m2','light attenuation due to suspended particulate solids',default=0.066_rk)

      call self%get_parameter(self%gammas1,'gammas1','day-1','mortality rate of small phytoplantkon',default=0.2_rk)
      call self%get_parameter(self%gammas2,'gammas2','day-1','mortality rate of diatom',default=0.075_rk)

      !zooplankton
      call self%get_parameter(self%beta1,'beta1','day-1','maximum grazing rate of microzooplankton',default=1.0_rk)
      call self%get_parameter(self%beta2,'beta2','day-1','maximum grazing rate of mesozooplankton',default=0.5_rk)

      call self%get_parameter(self%kgz1,'kgz1','mmol m-3','reference prey concentration for microzooplankton',default=0.5_rk)
      call self%get_parameter(self%kgz2,'kgz2','mmol m-3','reference prey concentration for mesozooplankton',default=0.25_rk)
      call self%get_parameter(self%rho1,'rho1','none','prey preference of diatom for mesozooplankton',default=0.6_rk)
      call self%get_parameter(self%rho2,'rho2','none','prey preference of miscrozooplankton for mesozooplankton',default=0.3_rk)
      call self%get_parameter(self%rho3,'rho3','none','prey preference of detritus nitrogen for mesozooplankton',default=0.1_rk)

      call self%get_parameter(self%gamma1,'gamma1','none','assimilation rate of microzooplankton',default=0.75_rk)
      call self%get_parameter(self%gamma2,'gamma2','none','assimilation rate of mesozooplankton',default=0.75_rk)
      call self%get_parameter(self%gammaz,'gammaz','day-1','mortality rate of zooplankton',default=0.2_rk)

      call self%get_parameter(self%kex1,'kex1','day-1','excretion rate of microzooplankton',default=0.2_rk)
      call self%get_parameter(self%kex2,'kex2','day-1','excretion rate of mesozooplankton',default=0.2_rk)

      !other
      call self%get_parameter(self%wss2,'wss2','m day-1','sinking rate of diatom',default=0.2_rk)
      call self%get_parameter(self%wsdn,'wsdn','m day-1','sinking rate of detritus nitrogen',default=1.0_rk)
      call self%get_parameter(self%wsdsi,'wsdsi','m day-1','sinking rate of detritus silicon',default=1.0_rk)

      call self%get_parameter(self%si2n,'si2n','none','silicon to nitrogen ratio',default=1.2_rk)
      call self%get_parameter(self%p2n,'p2n','none','phosphurus to nitrogen ratio',default=0.0625_rk)
      call self%get_parameter(self%o2no,'o2no','none','oxygen to nitrogen ratio for NO3 uptake',default=8.625_rk)
      call self%get_parameter(self%o2nh,'o2nh','none','oxygen to nitrogen ratio for NH4 uptake',default=6.625_rk)
      call self%get_parameter(self%c2n,'c2n','none','carbon to nitrogen',default=7.3_rk)

      call self%get_parameter(self%kox,'kox','mmol m-3','reference oxygen concentration of oxidation',default=30.0_rk)
      call self%get_parameter(self%kmdn1,'kmdn1','oC-1 day-1','temperature dependence for remineralization coefficients of detritus nitrogen',default=0.009_rk)
      call self%get_parameter(self%kmdn2,'kmdn2','day-1','base value for remineralization coefficients of detritus nitrogen',default=0.075_rk)
      call self%get_parameter(self%kmdsi1,'kmdsi1','oC-1 day-1','temperature dependence for remineralization coefficients of detritus silicon',default=0.0114_rk)
      call self%get_parameter(self%kmdsi2,'kmdsi2','day-1','base value for remineralization coefficients of detritus silicon',default=0.015_rk)
      call self%get_parameter(self%gamman,'gamman','day-1','nitrification coefficient',default=0.07_rk)
      call self%get_parameter(self%TR,'TR','oC','reference temperature for COSiNE kinetics',default=20.0_rk)
      call self%get_parameter(self%pco2a,'pco2a','ppm','atmospheric CO2 concentration',default=400.0_rk)

      call self%get_parameter(self%iz2graze,'iz2graze','none','flag for mesozooplankton grazing',default=1)

      call self%get_parameter(self%idapt,'idapt','none','flag for light adaption',default=0)
      call self%get_parameter(self%alpha_corr,'alpha_corr','none','factor for light adaption',default=1.25_rk)
      call self%get_parameter(self%zeptic,'zeptic','m','reference depth for light adaption',default=10.0_rk)

      call self%get_parameter(self%ico2s,'ico2s','none','flag for CO2 limitation on phytoplankton growth',default=0)
      call self%get_parameter(self%ndelay,'ndelay','day','number of days that mesozooplankton grazing is delayed',default=15.0_rk)
      call self%get_parameter(self%rdelay,'rdelay','none','relavative contribution of concentration at ndelay_th day',default=0.3_rk)
   
      !--------------------------------------------------------
      ! Register the contribution of all state variables to total nitrogen
      !--------------------------------------------------------
      call self%add_to_aggregate_variable(standard_variables%total_nitrogen, self%id_S1)
      call self%add_to_aggregate_variable(standard_variables%total_nitrogen, self%id_S2)
      call self%add_to_aggregate_variable(standard_variables%total_nitrogen, self%id_NO3)
      call self%add_to_aggregate_variable(standard_variables%total_nitrogen, self%id_NH4)
      call self%add_to_aggregate_variable(standard_variables%total_nitrogen, self%id_DN)

      !diagnostic variables
      call self%register_diagnostic_variable(self%id_PPR,  'PPR', 'mmol m-3 d-1', 'gross primary production rate')

      ! Register environmental dependencies
      call self%register_dependency(self%id_temp, standard_variables%temperature)
      call self%register_dependency(self%id_salt, standard_variables%practical_salinity)
      call self%register_dependency(self%id_zr,   standard_variables%depth)
      call self%register_dependency(self%id_dep,  standard_variables%cell_thickness)
      call self%register_dependency(self%id_PAR,  standard_variables%downwelling_photosynthetic_radiative_flux)

      ! Let phytoplankton (including background concentration) and detritus contribute to light attentuation
      call self%add_to_aggregate_variable(standard_variables%attenuation_coefficient_of_photosynthetic_radiative_flux, self%ak1)
      call self%add_to_aggregate_variable(standard_variables%attenuation_coefficient_of_photosynthetic_radiative_flux, self%id_S1, scale_factor=self%ak2)
      call self%add_to_aggregate_variable(standard_variables%attenuation_coefficient_of_photosynthetic_radiative_flux, self%id_S2, scale_factor=self%ak2)
      !constant SPM (todo: need add a varibles, check model%prepare_inputs() function)
      !call self%add_to_aggregate_variable(standard_variables%attenuation_coefficient_of_photosynthetic_radiative_flux, 20.0_rk, scale_factor=self%ak3)

   end subroutine initialize

   subroutine do(self, _ARGUMENTS_DO_)
      class (type_vims_cosine), intent(in) :: self
      _DECLARE_ARGUMENTS_DO_

      real(rk), parameter :: secs_pr_day = 86400.0_rk
      real(rk) :: NO3,SiO4,NH4,S1,S2,Z1,Z2,DN,DSi,PO4,DOX,CO2,ALK
      real(rk) :: temp,salt,zr,dep,PAR,qcos(13),mS2,mZ1,mDN,mZ2
      real(rk) :: rtmp

      !for precalculation
      real(rk) :: fS1,fS2,bfNO3S1,bfNH4S1,bfNH4S2,bfNO3S2
      real(rk) :: fNO3S1,fNH4S1,fNH4S2,fNO3S2,fPO4S1,fPO4S2,fCO2S1,fCO2S2,fSiO4S2
      real(rk) :: pnh4s1,pnh4s2,pih1,pih2,rhot,rhop,ADPT,OXR,Tadjust
    
      !for kinetics
      real(rk) :: NPS1,NPS2,RPS1,RPS2,SKS2,SKDN,SKDSi
      real(rk) :: MTS1,MTS2,MTZ1,MTZ2,EXZ1,EXZ2
      real(rk) :: GS1Z1,GS2Z2,GZ1Z2,GDNZ2,GTZ2
      real(rk) :: Nit,MIDN,MIDSi

      ! Enter spatial loops (if any)
      _LOOP_BEGIN_
         !-----------------------------------------------------------------
         ! Retrieve current (local) state variable values.
         !-----------------------------------------------------------------
         _GET_(self%id_NO3, NO3)
         _GET_(self%id_SiO4,SiO4)
         _GET_(self%id_NH4, NH4)
         _GET_(self%id_S1,  S1)
         _GET_(self%id_S2,  S2)
         _GET_(self%id_Z1,  Z1)
         _GET_(self%id_Z2,  Z2)
         _GET_(self%id_DN,  DN)
         _GET_(self%id_DSi, DSi)
         _GET_(self%id_PO4, PO4)
         _GET_(self%id_DOX, DOX)
         _GET_(self%id_CO2, CO2)
         _GET_(self%id_ALK, ALK)

         ! Retrieve current environmental conditions.
         ! _GET_SURFACE_(self%id_I_0,I_0)  ! surface photosynthetically active radiation
          _GET_(self%id_PAR,PAR)          ! local photosynthetically active radiation
          _GET_(self%id_temp,temp)      
          _GET_(self%id_salt,salt)        
          _GET_(self%id_zr,zr)        
          _GET_(self%id_dep,dep)        
         zr=-zr

         !-----------------------------------------------------------------
         !CoSiNE model kinetics: pre-calculation
         !-----------------------------------------------------------------
         !temperature adjust          
         Tadjust=exp(0.069_rk*(temp-self%TR))
        
         !Light limitation factor including photo-inhibition and light adaptation
         ADPT=1.0_rk
         if (self%idapt==1) ADPT=self%alpha_corr*(1.0_rk-4.0_rk*zr)/self%zeptic
         pih1=(1.0_rk-exp(-PAR*ADPT*self%alpha1/self%gmaxs1))*exp(-self%beta*PAR/self%gmaxs1)
         pih2=(1.0_rk-exp(-PAR*ADPT*self%alpha2/self%gmaxs2))*exp(-self%beta*PAR/self%gmaxs2)

         !NH4 inhibition 
         pnh4s1=min(1.0_rk,exp(-self%pis1*NH4))
         pnh4s2=min(1.0_rk,exp(-self%pis2*NH4))
    
         !PO4, CO2 and SiO4 limitation factor
         fPO4S1=PO4/(self%kpo4s1+PO4)
         fCO2S1=CO2/(self%kco2s1+CO2)
         fPO4S2=PO4/(self%kpo4s2+PO4)
         fCO2S2=CO2/(self%kco2s2+CO2)
         fSiO4S2=SiO4/(self%ksio4s2+SiO4)

         !nitrogen limitation factor
         rtmp=1.0_rk+NH4/self%knh4s1+pnh4s1*NO3/self%kno3s1
         bfNO3S1=pnh4s1*NO3/(self%kno3s1*rtmp)
         bfNH4S1=NH4/(self%knh4s1*rtmp)

         rtmp=1.0_rk+NH4/self%knh4s2+pnh4s2*NO3/self%kno3s2
         bfNO3S2=pnh4s2*NO3/(self%kno3s2*rtmp)
         bfNH4S2=NH4/(self%knh4s2*rtmp)

         !final limitation
         if(self%ico2s==0) then
           fS1=min(bfNO3S1+bfNH4S1,fPO4S1)
           fS2=min(bfNO3S2+bfNH4S2,fSiO4S2,fPO4S2)
         else
           fS1=min(bfNO3S1+bfNH4S1,fPO4S1,fCO2S1)
           fS2=min(bfNO3S2+bfNH4S2,fSiO4S2,fPO4S2,fCO2S2)
         endif
       
         !adjustment for nitrogen limitation factors
         fNO3S1=fS1*bfNO3S1/(bfNO3S1+bfNH4S1+1.0e-6)
         fNH4S1=fS1*bfNH4S1/(bfNO3S1+bfNH4S1+1.0e-6)

         fNO3S2=fS2*bfNO3S2/(bfNO3S2+bfNH4S2+1.0E-6)
         fNH4S2=fS2*bfNH4S2/(bfNO3S2+bfNH4S2+1.0E-6)
 
         !zooplankton grazing  
         GS1Z1=self%beta1*Z1*S1/(self%kgz1+S1)
         if(S1<=0.25_rk) GS1Z1=0.0_rk

         mS2=S2; mZ1=Z1; mDN=DN; mZ2=Z2   !todo: need to add option for ndelay!=0
         rhot=self%rho1*mS2+self%rho2*mZ1+self%rho3*mDN
         rhop=self%rho1*mS2*mS2+self%rho2*mZ1*mZ1+self%rho3*mDN*mDN

         GS2Z2=self%beta2*self%rho1*mS2*mS2*mZ2/(self%kgz2*rhot+rhop)
         GZ1Z2=self%beta2*self%rho2*mZ1*mZ1*mZ2/(self%kgz2*rhot+rhop)
         GDNZ2=self%beta2*self%rho3*mDN*mDN*mZ2/(self%kgz2*rhot+rhop)

         !turn off mesozooplankton grazing at certain conditions
         if((rhot<=0.0_rk .and. rhop<= 0.0_rk) .or. self%iz2graze==0) then
           GS2Z2=0.0_rk; GZ1Z2=0.0_rk; GDNZ2=0.0_rk
         endif
         if (mS2<=0.5_rk) GS2Z2=0.0_rk
         if (mZ1<=0.025_rk) GZ1Z2=0.0_rk

         GTZ2=GS2Z2+GZ1Z2+GS2Z2

         !oxidation 
         OXR=DOX/(self%kox+DOX)

         !-----------------------------------------------------------------
         !CoSiNE model kinetics: computing the reaction rate 
         !-----------------------------------------------------------------
         !S1
         NPS1=self%gmaxs1*fNO3S1*pih1*S1
         RPS1=self%gmaxs1*max(self%kns1*NH4/(self%knh4s1+NH4),fNH4S1*pih1)*S1
         MTS1=self%gammas1*S1
         qcos(4)=NPS1+RPS1-GS1Z1-MTS1

         !S2
         NPS2=self%gmaxs2*fNO3S2*pih2*S2
         RPS2=self%gmaxs2*max(self%kns2*NH4/(self%knh4s2+NH4),fNH4S2*pih2)*S2
         MTS2=self%gammas2*S2
         qcos(5)=NPS2+RPS2-GS2Z2-MTS2

         !Z1
         EXZ1=self%kex1*OXR*Z1
         MTZ1=self%gammaz*Z1*Z1
         qcos(6)=self%gamma1*GS1Z1-EXZ1-GZ1Z2-MTZ1
        
         !Z2
         EXZ2=self%kex2*OXR*Z2 
         MTZ2=self%gammaz*Z2*Z2
         qcos(7)=self%gamma2*GTZ2-EXZ2-MTZ2

         !DN
         MIDN=max(self%kmdn1*temp+self%kmdn2,0.05_rk)*OXR*DN
         qcos(8)=(1.0_rk-self%gamma1)*GS1Z1+(1.0_rk-self%gamma2)*GTZ2-GDNZ2 &
                & +MTS1+MTS2+MTZ1+MTZ2-MIDN

         !DSi
         MIDSi=max(self%kmdsi1*temp+self%kmdsi2,0.01_rk)*DSi
         qcos(9)=(GS2Z2+MTS2)*self%si2n-MIDSi

         !NO3
         Nit=self%gamman*OXR*NH4
         qcos(1)=-NPS1-NPS2+Nit

         !NH4
         qcos(3)=-RPS1-RPS2+EXZ1+EXZ2-Nit+MIDN

         !SiO4
         qcos(2)=-(NPS2+RPS2)*self%si2n+MIDSi

         !PO4
         qcos(10)=(EXZ1+EXZ2+MIDN+MIDSi/self%si2n-NPS1-RPS1-NPS2-RPS2)*self%p2n

         !DOX       
         qcos(11)=(NPS1+NPS2)*self%o2no+(RPS1+RPS2-EXZ1-EXZ2-MIDN)*self%o2nh-2.0_rk*Nit

         !CO2
         qcos(12)=(EXZ1+EXZ2+MIDN-NPS1-RPS1-NPS2-RPS2)*self%c2n

         !ALK
         qcos(13)=-qcos(1)+qcos(3)

         !temperatue adjustl
         qcos=qcos*Tadjust

         !bottom and surface fluxes (todo)

         !-----------------------------------------------------------------
         ! Set temporal derivatives
         !-----------------------------------------------------------------
         _ADD_SOURCE_(self%id_NO3, qcos(1)/secs_pr_day)
         _ADD_SOURCE_(self%id_SiO4,qcos(2)/secs_pr_day)
         _ADD_SOURCE_(self%id_NH4, qcos(3)/secs_pr_day)
         _ADD_SOURCE_(self%id_S1,  qcos(4)/secs_pr_day)
         _ADD_SOURCE_(self%id_S2,  qcos(5)/secs_pr_day)
         _ADD_SOURCE_(self%id_Z1,  qcos(6)/secs_pr_day)
         _ADD_SOURCE_(self%id_Z2,  qcos(7)/secs_pr_day)
         _ADD_SOURCE_(self%id_DN,  qcos(8)/secs_pr_day)
         _ADD_SOURCE_(self%id_DSi, qcos(9)/secs_pr_day)
         _ADD_SOURCE_(self%id_PO4, qcos(10)/secs_pr_day)
         _ADD_SOURCE_(self%id_DOX, qcos(11)/secs_pr_day)
         _ADD_SOURCE_(self%id_CO2, qcos(12)/secs_pr_day)
         _ADD_SOURCE_(self%id_ALK, qcos(13)/secs_pr_day)

      _LOOP_END_
   end subroutine do

   subroutine do_ppdd(self, _ARGUMENTS_DO_PPDD_)
      class (type_vims_cosine), intent(in) :: self
      _DECLARE_ARGUMENTS_DO_PPDD_

      real(rk)            :: no3
      real(rk), parameter :: secs_pr_day = 86400.0_rk

      ! Enter spatial loops (if any)
      _LOOP_BEGIN_

         ! Retrieve current (local) state variable values.
         !_GET_(self%id_no3, no3) ! nutrient

         ! Retrieve current environmental conditions.
         ! _GET_(self%id_par,par)          ! local photosynthetically active radiation
         ! _GET_SURFACE_(self%id_I_0,I_0)  ! surface photosynthetically active radiation

         ! Assign destruction rates to different elements of the destruction matrix.
         ! By assigning with _SET_DD_SYM_(i,j,val) as opposed to _SET_DD_(i,j,val),
         ! assignments to dd(i,j) are automatically assigned to pp(j,i) as well.
         !_SET_DD_SYM_(self%id_no3,self%id_p,primprod)                           ! snp

      _LOOP_END_
   end subroutine do_ppdd

end module vims_cosine
