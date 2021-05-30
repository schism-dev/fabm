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
      type (type_dependency_id) :: id_temp,id_salt,id_PAR
     
      !dianostic
      type (type_diagnostic_variable_id) :: id_PPR

      !model parameters
      real(rk) :: gmaxs1,gmaxs2,pis1,pis2,kno3s1,knh4s1,kpo4s1,kco2s1,kno3s2,knh4s2
      real(rk) :: kpo4s2,kco2s2,ksio4s2,alpha1,alpha2,ak1,ak2,ak3,beta,gammas1,gammas2
      real(rk) :: beta1,beta2,kgz1,kgz2,rho1,rho2,rho3,gamma1,gamma2,gammaz,kex1,kex2,wss2,wsdn,wsdsi
      real(rk) :: si2n,p2n,o2no,o2nh,c2n,kox,kmdn1,kmdn2,kmdsi1,kmdsi2,gamman,TR,pco2a

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
      call self%register_dependency(self%id_salt, standard_variables%practical_salinity)
      call self%register_dependency(self%id_temp, standard_variables%temperature)
      call self%register_dependency(self%id_PAR, standard_variables%downwelling_photosynthetic_radiative_flux)

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
      real(rk) :: PAR,temp

      ! Enter spatial loops (if any)
      _LOOP_BEGIN_

         ! Retrieve current (local) state variable values.
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
          _GET_(self%id_PAR,PAR)          ! local photosynthetically active radiation
          _GET_(self%id_temp,temp)          ! local photosynthetically active radiation
         ! _GET_SURFACE_(self%id_I_0,I_0)  ! surface photosynthetically active radiation

         ! Set temporal derivatives
         _ADD_SOURCE_(self%id_NO3, 0)
         _ADD_SOURCE_(self%id_SiO4,0)
         _ADD_SOURCE_(self%id_NH4, 0)
         _ADD_SOURCE_(self%id_S1,  0)
         _ADD_SOURCE_(self%id_S2,  0)
         _ADD_SOURCE_(self%id_Z1,  0)
         _ADD_SOURCE_(self%id_Z2,  0)
         _ADD_SOURCE_(self%id_DN,  0)
         _ADD_SOURCE_(self%id_DSi, 0)
         _ADD_SOURCE_(self%id_PO4, 0)
         _ADD_SOURCE_(self%id_DOX, 0)
         _ADD_SOURCE_(self%id_CO2, 0)
         _ADD_SOURCE_(self%id_ALK, 0)

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
