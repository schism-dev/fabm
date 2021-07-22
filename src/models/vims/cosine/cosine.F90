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
   use fabm_cosine_misc

   implicit none

   private

   type, extends(type_base_model), public :: type_vims_cosine
      !variable identifiers
      type (type_state_variable_id) :: id_NO3, id_SiO4, id_NH4
      type (type_state_variable_id) :: id_S1,  id_S2,   id_Z1,  id_Z2
      type (type_state_variable_id) :: id_DN,  id_DSi,  id_PO4
      type (type_state_variable_id) :: id_DOX, id_CO2,  id_ALK

      type (type_bottom_state_variable_id) :: id_PS21,id_PS22,id_PDN1,id_PDN2,id_PDSi
      type (type_bottom_state_variable_id) :: id_RS21,id_RS22,id_RDN1,id_RDN2,id_RDSi

      !dependence
      type (type_dependency_id) :: id_temp,id_salt,id_dep,id_zr,id_Ke,id_PAR,id_SPM
      type (type_surface_dependency_id) :: id_PAR0,id_Uw
     
      !dianostic
      type (type_diagnostic_variable_id) :: id_dPAR
      type (type_surface_diagnostic_variable_id) :: id_stmp
      type (type_bottom_diagnostic_variable_id)  :: id_bTN

      type (type_diagnostic_variable_id) :: id_TN,id_PPR
      type (type_diagnostic_variable_id) :: id_NPS1,id_RPS1,id_MTS1,id_GS1Z1
      type (type_diagnostic_variable_id) :: id_NPS2,id_RPS2,id_MTS2,id_GS2Z2
      type (type_diagnostic_variable_id) :: id_EXZ1,id_MTZ1,id_EXZ2,id_MTZ2
      type (type_diagnostic_variable_id) :: id_MIDN,id_Nit,id_GDNZ2,id_GZ1Z2,id_GTZ2

      !model parameters
      integer  :: idapt,ico2s,iz2graze,ispm,ipo4
      real(rk) :: dts,gmaxs1,gmaxs2,pis1,pis2,kno3s1,knh4s1,kpo4s1,kco2s1,kno3s2,knh4s2
      real(rk) :: kpo4s2,kco2s2,ksio4s2,kns1,kns2,alpha1,alpha2,ak1,ak2,ak3,beta,gammas1,gammas2
      real(rk) :: beta1,beta2,kgz1,kgz2,rho1,rho2,rho3,gamma1,gamma2,gammaz,kex1,kex2,wss2,wsdn,wsdsi
      real(rk) :: si2n,p2n,o2no,o2nh,c2n,kox,kmdn1,kmdn2,kmdsi1,kmdsi2,gamman,TR,pco2a
      real(rk) :: alpha_corr,zeptic,ndelay,rdelay,spm0,fpar
      real(rk) :: fS21,fS22,fDN1,fDN2,fDSi,rkS21,rkS22,rkDN1,rkDN2,rkDSi,mkS21,mkS22,mkDN1,mkDN2,mkDSi

   contains
      procedure :: initialize
      procedure :: do
      procedure :: do_ppdd
      procedure :: do_column
      procedure :: get_vertical_movement
      procedure :: do_surface
      procedure :: do_bottom
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

      !for sediment module
      call self%register_state_variable(self%id_PS21, 'PS21', 'mmol m-2','Sediment S2 Conc. G1', 1.e1_rk, minimum=0.0_rk)
      call self%register_state_variable(self%id_PS22, 'PS22', 'mmol m-2','Sediment S2 Conc. G2', 1.e1_rk, minimum=0.0_rk)
      call self%register_state_variable(self%id_PDN1, 'PDN1', 'mmol m-2','Sediment DN Conc. G1', 1.e1_rk, minimum=0.0_rk)
      call self%register_state_variable(self%id_PDN2, 'PDN2', 'mmol m-2','Sediment DN Conc. G2', 1.e1_rk, minimum=0.0_rk)
      call self%register_state_variable(self%id_PDSi, 'PDSi', 'mmol m-2','Sediment DSi Conc.',   1.e1_rk, minimum=0.0_rk)

      call self%register_state_variable(self%id_RS21, 'RS21', 'day-1','Sediment S2 decay rate',  0.1_rk,  minimum=0.0_rk,maximum=1.0_rk)
      call self%register_state_variable(self%id_RS22, 'RS22', 'day-1','Sediment S2 decay rate',  0.01_rk, minimum=0.0_rk,maximum=1.0_rk)
      call self%register_state_variable(self%id_RDN1, 'RDN1', 'day-1','Sediment DN decay rate',  0.1_rk,  minimum=0.0_rk,maximum=1.0_rk)
      call self%register_state_variable(self%id_RDN2, 'RDN2', 'day-1','Sediment DN decay rate',  0.01_rk, minimum=0.0_rk,maximum=1.0_rk)
      call self%register_state_variable(self%id_RDSi, 'RDSi', 'day-1','Sediment DSi decay rate', 0.1_rk,  minimum=0.0_rk,maximum=1.0_rk)

      !--------------------------------------------------------
      !Note: parameter values in our own derived type
      !1). all rates must be provided in values per day in the configuration file,
      !2). are converted here to values per second by specifying scale_factor=d_per_s.
      !--------------------------------------------------------

      call self%get_parameter(self%dts,   'dts',   'seconds','time step of the model',default=120.0_rk)
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

      call self%get_parameter(self%fpar,'fpar','none','par fraction of short-wave solar radiation',default=0.46_rk)
      call self%get_parameter(self%ispm,'ispm','none','flag for SPM specification',default=0)
      call self%get_parameter(self%spm0,'spm0','mg/L','constant for SPM concentration for ispm=0',default=10.0_rk)

      call self%get_parameter(self%ipo4,'ipo4','none','flag for additional PO4 from silicon dissolution',default=0)

      call self%get_parameter(self%fS21, 'fS21','none','S2 G1 fraction into sediment ',default=0.1_rk)
      call self%get_parameter(self%fS22, 'fS22','none','S2 G2 fraction into sediment ',default=0.1_rk)
      call self%get_parameter(self%fDN1, 'fDN1','none','DN G1 fraction into sediment ',default=0.15_rk)
      call self%get_parameter(self%fDN2, 'fDN2','none','DN G2 fraction into sediment ',default=0.1_rk)
      call self%get_parameter(self%fDSi, 'fDSi','none','DSi fraction into sediment ',  default=1.0_rk)

      call self%get_parameter(self%rkS21, 'rkS21','none','change rate of S2 G1 fraction',default=4e-3_rk)
      call self%get_parameter(self%rkS22, 'rkS22','none','change rate of S2 G2 fraction',default=1e-4_rk)
      call self%get_parameter(self%rkDN1, 'rkDN1','none','change rate of DN G1 fraction',default=4e-3_rk)
      call self%get_parameter(self%rkDN2, 'rkDN2','none','change rate of DN G2 fraction',default=1e-4_rk)
      call self%get_parameter(self%rkDSi, 'rkDSi','none','change rate of DSi   fraction',default=4e-3_rk)

      call self%get_parameter(self%mkS21, 'mkS21','none','maximum decay rate of S2 G1 fraction',default=0.1_rk)
      call self%get_parameter(self%mkS22, 'mkS22','none','maximum decay rate of S2 G2 fraction',default=0.01_rk)
      call self%get_parameter(self%mkDN1, 'mkDN1','none','maximum decay rate of DN G1 fraction',default=0.1_rk)
      call self%get_parameter(self%mkDN2, 'mkDN2','none','maximum decay rate of DN G2 fraction',default=0.01_rk)
      call self%get_parameter(self%mkDSi, 'mkDSi','none','maximum decay rate of DSi   fraction',default=0.1_rk)

      !--------------------------------------------------------
      ! Register the contribution of all state variables to total nitrogen
      !--------------------------------------------------------
      call self%add_to_aggregate_variable(standard_variables%total_nitrogen, self%id_S1)
      call self%add_to_aggregate_variable(standard_variables%total_nitrogen, self%id_S2)
      call self%add_to_aggregate_variable(standard_variables%total_nitrogen, self%id_Z1)
      call self%add_to_aggregate_variable(standard_variables%total_nitrogen, self%id_Z2)
      call self%add_to_aggregate_variable(standard_variables%total_nitrogen, self%id_NO3)
      call self%add_to_aggregate_variable(standard_variables%total_nitrogen, self%id_NH4)
      call self%add_to_aggregate_variable(standard_variables%total_nitrogen, self%id_DN)

      !diagnostic variables
      call self%register_diagnostic_variable(self%id_dPAR, 'dPAR','W m-2', 'photosynthetic_radiative_flux', missing_value=0.0_rk,&
           & standard_variable=standard_variables%downwelling_photosynthetic_radiative_flux, source=source_do_column)

      call self%register_diagnostic_variable(self%id_stmp,  'stmp', 'none',         'temporary variables',    output=output_none)
      call self%register_diagnostic_variable(self%id_PPR,   'PPR',  'mmol m-3 d-1', 'primary production rate',output=output_none)
      call self%register_diagnostic_variable(self%id_bTN,   'bTN',  'mmol m-2',     'bTN'  , output=output_none)
      call self%register_diagnostic_variable(self%id_TN,    'TN',   'mmol m-3',     'TN'   , output=output_none)
      call self%register_diagnostic_variable(self%id_NPS1,  'NPS1', 'mmol m-3 d-1', 'NPS1' , output=output_none)
      call self%register_diagnostic_variable(self%id_RPS1,  'RPS1', 'mmol m-3 d-1', 'RPS1' , output=output_none)
      call self%register_diagnostic_variable(self%id_MTS1,  'MTS1', 'mmol m-3 d-1', 'MTS1' , output=output_none)
      call self%register_diagnostic_variable(self%id_GS1Z1, 'GS1Z1','mmol m-3 d-1', 'GS1Z1', output=output_none)
      call self%register_diagnostic_variable(self%id_EXZ1,  'EXZ1', 'mmol m-3 d-1', 'EXZ1' , output=output_none)
      call self%register_diagnostic_variable(self%id_MTZ1,  'MTZ1', 'mmol m-3 d-1', 'MTZ1' , output=output_none)
      call self%register_diagnostic_variable(self%id_NPS2,  'NPS2', 'mmol m-3 d-1', 'NPS2' , output=output_none)
      call self%register_diagnostic_variable(self%id_RPS2,  'RPS2', 'mmol m-3 d-1', 'RPS2' , output=output_none)
      call self%register_diagnostic_variable(self%id_MTS2,  'MTS2', 'mmol m-3 d-1', 'MTS2' , output=output_none)
      call self%register_diagnostic_variable(self%id_GS2Z2, 'GS2Z2','mmol m-3 d-1', 'GS2Z2', output=output_none)
      call self%register_diagnostic_variable(self%id_EXZ2,  'EXZ2', 'mmol m-3 d-1', 'EXZ2' , output=output_none)
      call self%register_diagnostic_variable(self%id_MTZ2,  'MTZ2', 'mmol m-3 d-1', 'MTZ2' , output=output_none)
      call self%register_diagnostic_variable(self%id_MIDN,  'MIDN', 'mmol m-3 d-1', 'MIDN' , output=output_none)
      call self%register_diagnostic_variable(self%id_Nit,   'Nit',  'mmol m-3 d-1', 'Nit'  , output=output_none)
      call self%register_diagnostic_variable(self%id_GDNZ2, 'GDNZ2','mmol m-3 d-1', 'GDNZ2', output=output_none)
      call self%register_diagnostic_variable(self%id_GZ1Z2, 'GZ1Z2','mmol m-3 d-1', 'GZ1Z2', output=output_none)
      call self%register_diagnostic_variable(self%id_GTZ2,  'GTZ2', 'mmol m-3 d-1', 'GTZ2' , output=output_none)

      ! Register environmental dependencies
      call self%register_dependency(self%id_temp, standard_variables%temperature)
      call self%register_dependency(self%id_salt, standard_variables%practical_salinity)
      call self%register_dependency(self%id_zr,   standard_variables%depth)
      call self%register_dependency(self%id_dep,  standard_variables%cell_thickness)
      call self%register_dependency(self%id_Ke,   standard_variables%attenuation_coefficient_of_photosynthetic_radiative_flux)
      call self%register_dependency(self%id_PAR,  standard_variables%downwelling_photosynthetic_radiative_flux)
      call self%register_dependency(self%id_SPM,  standard_variables%mass_concentration_of_suspended_matter)

      call self%register_dependency(self%id_PAR0, standard_variables%surface_downwelling_photosynthetic_radiative_flux)
      call self%register_dependency(self%id_Uw,   standard_variables%wind_speed)

      ! Let phytoplankton (including background concentration) and detritus contribute to light attentuation
      call self%add_to_aggregate_variable(standard_variables%attenuation_coefficient_of_photosynthetic_radiative_flux, self%ak1)
      call self%add_to_aggregate_variable(standard_variables%attenuation_coefficient_of_photosynthetic_radiative_flux, self%id_S1, scale_factor=self%ak2)
      call self%add_to_aggregate_variable(standard_variables%attenuation_coefficient_of_photosynthetic_radiative_flux, self%id_S2, scale_factor=self%ak2)
      call self%add_to_aggregate_variable(standard_variables%attenuation_coefficient_of_photosynthetic_radiative_flux, self%id_SPM, scale_factor=1000.0_rk*self%ak3)
      !constant SPM (todo: need add a varibles, check model%prepare_inputs() function )
      !call self%add_to_aggregate_variable(standard_variables%attenuation_coefficient_of_photosynthetic_radiative_flux, 20.0_rk, scale_factor=self%ak3)

   end subroutine initialize

   subroutine do(self, _ARGUMENTS_DO_)
      class (type_vims_cosine), intent(in) :: self
      _DECLARE_ARGUMENTS_DO_

      real(rk), parameter :: secs_pr_day = 86400.0_rk, mval=1e-10_rk
      real(rk) :: NO3,SiO4,NH4,S1,S2,Z1,Z2,DN,DSi,PO4,DOX,CO2,ALK
      real(rk) :: temp,salt,zr,dep,PAR,qcos(13),mcos(13),mS2,mZ1,mDN,mZ2
      real(rk) :: rtmp
      integer  :: i

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
         pih1=(1.0_rk-exp(-PAR*ADPT*self%alpha1/max(self%gmaxs1,mval)))*exp(-self%beta*PAR/max(self%gmaxs1,mval))
         pih2=(1.0_rk-exp(-PAR*ADPT*self%alpha2/max(self%gmaxs2,mval)))*exp(-self%beta*PAR/max(self%gmaxs2,mval))

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

         GTZ2=GS2Z2+GZ1Z2+GDNZ2

         !oxidation 
         OXR=DOX/(self%kox+DOX)

         !-----------------------------------------------------------------
         !CoSiNE model kinetics: computing the reaction rate 
         !-----------------------------------------------------------------
         !S1
         NPS1=self%gmaxs1*fNO3S1*pih1*S1
         RPS1=self%gmaxs1*max(self%kns1*NH4/(self%knh4s1+NH4),fNH4S1*pih1)*S1
         MTS1=self%gammas1*S1

         !S2
         NPS2=self%gmaxs2*fNO3S2*pih2*S2
         RPS2=self%gmaxs2*max(self%kns2*NH4/(self%knh4s2+NH4),fNH4S2*pih2)*S2
         MTS2=self%gammas2*S2

         !check growth term  
         if(NH4<(RPS1+RPS2)*self%dts/secs_pr_day) then
           RPS1=0.0_rk; RPS2=0.0_rk
         endif

         if(NO3<(NPS1+NPS2)*self%dts/secs_pr_day) then
           NPS1=0.0_rk; NPS2=0.0_rk
         endif

         if(SiO4<(RPS2+NPS2)*self%si2n*self%dts/secs_pr_day) then
           RPS2=0.0_rk; NPS2=0.0_rk
         endif

         if(PO4<(RPS1+NPS1+RPS2+NPS2)*self%p2n*self%dts/secs_pr_day) then
           RPS1=0.0_rk; NPS1=0.0_rk; RPS2=0.0_rk; NPS2=0.0_rk
         endif

         qcos(4)=NPS1+RPS1-GS1Z1-MTS1
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
         qcos(10)=(EXZ1+EXZ2+MIDN-NPS1-RPS1-NPS2-RPS2)*self%p2n
         if(self%ipo4==1) qcos(10)=qcos(10)+MIDSi*self%p2n/self%si2n

         !DOX       
         qcos(11)=(NPS1+NPS2)*self%o2no+(RPS1+RPS2-EXZ1-EXZ2-MIDN)*self%o2nh-2.0_rk*Nit

         !CO2
         qcos(12)=(EXZ1+EXZ2+MIDN-NPS1-RPS1-NPS2-RPS2)*self%c2n

         !ALK
         qcos(13)=-qcos(1)+qcos(3)

         !temperatue adjustl
         qcos=qcos*Tadjust

         !change the units of reaction rates
         qcos=qcos/secs_pr_day
         
         !set reaction rates to zero if corant number condition is violated
         mcos(1)  = NO3  /self%dts
         mcos(2)  = SiO4 /self%dts
         mcos(3)  = NH4  /self%dts
         mcos(4)  = S1   /self%dts
         mcos(5)  = S2   /self%dts
         mcos(6)  = Z1   /self%dts
         mcos(7)  = Z2   /self%dts
         mcos(8)  = DN   /self%dts
         mcos(9)  = DSi  /self%dts
         mcos(10) = PO4  /self%dts
         mcos(11) = DOX  /self%dts
         mcos(12) = CO2  /self%dts
         mcos(13) = ALK  /self%dts

         do i=1,13
           if(qcos(i)+mcos(i)<0) then
             qcos(i)=-mcos(i)
           endif
         enddo

         !-----------------------------------------------------------------
         ! Set temporal derivatives
         !-----------------------------------------------------------------
         _ADD_SOURCE_(self%id_NO3, qcos(1))
         _ADD_SOURCE_(self%id_SiO4,qcos(2))
         _ADD_SOURCE_(self%id_NH4, qcos(3))
         _ADD_SOURCE_(self%id_S1,  qcos(4))
         _ADD_SOURCE_(self%id_S2,  qcos(5))
         _ADD_SOURCE_(self%id_Z1,  qcos(6))
         _ADD_SOURCE_(self%id_Z2,  qcos(7))
         _ADD_SOURCE_(self%id_DN,  qcos(8))
         _ADD_SOURCE_(self%id_DSi, qcos(9))
         _ADD_SOURCE_(self%id_PO4, qcos(10))
         _ADD_SOURCE_(self%id_DOX, qcos(11))
         _ADD_SOURCE_(self%id_CO2, qcos(12))
         _ADD_SOURCE_(self%id_ALK, qcos(13))

         !set diagnostic variables
         !_SET_DIAGNOSTIC_(self%id_Light,PAR)
         _SET_DIAGNOSTIC_(self%id_TN, S1+S2+Z1+Z2+NO3+NH4+DN)
         _SET_DIAGNOSTIC_(self%id_NPS1, NPS1)
         _SET_DIAGNOSTIC_(self%id_RPS1, RPS1)
         _SET_DIAGNOSTIC_(self%id_MTS1, MTS1)
         _SET_DIAGNOSTIC_(self%id_GS1Z1,GS1Z1)
         _SET_DIAGNOSTIC_(self%id_EXZ1, EXZ1)
         _SET_DIAGNOSTIC_(self%id_MTZ1, MTZ1)
         _SET_DIAGNOSTIC_(self%id_NPS2, NPS2)
         _SET_DIAGNOSTIC_(self%id_RPS2, RPS2)
         _SET_DIAGNOSTIC_(self%id_MTS2, MTS2)
         _SET_DIAGNOSTIC_(self%id_GS2Z2,GS2Z2)
         _SET_DIAGNOSTIC_(self%id_EXZ2, EXZ2)
         _SET_DIAGNOSTIC_(self%id_MTZ2, MTZ2)
         _SET_DIAGNOSTIC_(self%id_MIDN, MIDN)
         _SET_DIAGNOSTIC_(self%id_Nit,  Nit)
         _SET_DIAGNOSTIC_(self%id_GDNZ2,GDNZ2)
         _SET_DIAGNOSTIC_(self%id_GZ1Z2,GZ1Z2)
         _SET_DIAGNOSTIC_(self%id_GTZ2, GTZ2)

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

   subroutine do_column(self, _ARGUMENTS_DO_COLUMN_)
      class (type_vims_cosine), intent(in) :: self
      _DECLARE_ARGUMENTS_DO_COLUMN_

      real(rk) :: dep,Ke,PAR0,PAR,dPAR
      real(rk) :: tPAR

      _GET_SURFACE_(self%id_PAR0,PAR0)
      tPAR=PAR0

      _DOWNWARD_LOOP_BEGIN_
         _GET_(self%id_dep,dep)     ! Layer height (m)
         _GET_(self%id_Ke,Ke)       ! PAR attenuation (m-1)
 
         !compute PAR at layer center and layer bottom
         dPAR=tPAR*exp(-Ke*dep/2.0_rk)
         tPAR=tPAR*exp(-Ke*dep)

         _SET_DIAGNOSTIC_(self%id_dPAR,dPAR) ! Photosynthetically active radiation at layer centre
      _DOWNWARD_LOOP_END_
   end subroutine do_column

   subroutine get_vertical_movement(self, _ARGUMENTS_GET_VERTICAL_MOVEMENT_)
      class (type_vims_cosine), intent(in) :: self
      _DECLARE_ARGUMENTS_GET_VERTICAL_MOVEMENT_

      real(rk), parameter :: secs_pr_day = 86400.0_rk
      real(rk) :: dep,S2,rat,drat
   
      _LOOP_BEGIN_
        _GET_(self%id_dep,dep)     ! Layer height (m)
        _GET_(self%id_S2,S2)       ! diatom conc. 

        drat=dep/max(dep,0.1_rk); rat=1.0_rk
        if(S2<=2.5_rk) rat=0.0_rk
        _ADD_VERTICAL_VELOCITY_(self%id_S2,  -rat*drat*self%wss2/secs_pr_day)
        _ADD_VERTICAL_VELOCITY_(self%id_DN,  -drat*self%wsdn/secs_pr_day)
        _ADD_VERTICAL_VELOCITY_(self%id_DSi, -drat*self%wsdsi/secs_pr_day)
      _LOOP_END_
   end subroutine get_vertical_movement

   subroutine do_surface(self, _ARGUMENTS_DO_SURFACE_)
      class (type_vims_cosine), intent(in) :: self
      _DECLARE_ARGUMENTS_DO_SURFACE_

      !local variables
      real(rk), parameter :: secs_pr_day = 86400.0_rk
      real(rk) :: dep,temp,salt,DOX,CO2,SiO4,PO4,ALK,Uw
      real(rk) :: rat,drat,ph,o2flx,co2flx
   
      _SURFACE_LOOP_BEGIN_
         _GET_(self%id_dep, dep)
         _GET_(self%id_temp,temp)
         _GET_(self%id_salt,salt)
         _GET_(self%id_DOX, DOX)
         _GET_(self%id_CO2, CO2)
         _GET_(self%id_SiO4,SiO4)
         _GET_(self%id_PO4, PO4)
         _GET_(self%id_ALK, ALK)
         _GET_SURFACE_(self%id_Uw,Uw)

         rat=1.e-6_rk; drat=dep/max(dep,0.1_rk)
         !for O2 air-sea exchange
         call o2flux(o2flx,temp,salt,DOX,Uw)
         _ADD_SURFACE_FLUX_(self%id_DOX,drat*o2flx/secs_pr_day)

         !for CO2 air-sea exchange
         call co2flux(2,ph,co2flx,temp,salt,CO2*rat,SiO4*rat,PO4*rat, ALK*rat,self%pco2a,Uw)
         _ADD_SURFACE_FLUX_(self%id_CO2,drat*co2flx/secs_pr_day)

         !set diagnostic variable 
         _SET_SURFACE_DIAGNOSTIC_(self%id_stmp,drat*co2flx)

      _SURFACE_LOOP_END_
   end subroutine do_surface

   subroutine do_bottom(self, _ARGUMENTS_DO_BOTTOM_)
      !reflective sediment flux model with accumulative POM and variable decay rate
      class (type_vims_cosine),intent(in) :: self
      _DECLARE_ARGUMENTS_DO_BOTTOM_
      real(rk), parameter :: secs_pr_day = 86400.0_rk, mval=1e-10_rk
      real(rk) :: S2,DN,DSi,dep,rat,drat
      real(rk) :: FS21,FS22,FDN1,FDN2,FDSi,JS21,JS22,JDN1,JDN2,JDSi  !depositional fluxes, and diagenesis fluxes
      real(rk) :: PS21,PS22,PDN1,PDN2,PDSi,RS21,RS22,RDN1,RDN2,RDSi  !sediment POM conc., and decay rate
      real(rk) :: nh4flx,sio4flx,po4flx,co2flx,o2flx                 !sediment nutrient fluxes
   
      _BOTTOM_LOOP_BEGIN_
         _GET_(self%id_dep,dep)
         _GET_(self%id_S2, S2)
         _GET_(self%id_DN, DN)
         _GET_(self%id_DSi,DSi)
         _GET_BOTTOM_(self%id_PS21, PS21)
         _GET_BOTTOM_(self%id_PS22, PS22)
         _GET_BOTTOM_(self%id_PDN1, PDN1)
         _GET_BOTTOM_(self%id_PDN2, PDN2)
         _GET_BOTTOM_(self%id_PDSi, PDSi)
         _GET_BOTTOM_(self%id_RS21, RS21)
         _GET_BOTTOM_(self%id_RS22, RS22)
         _GET_BOTTOM_(self%id_RDN1, RDN1)
         _GET_BOTTOM_(self%id_RDN2, RDN2)
         _GET_BOTTOM_(self%id_RDSi, RDSi)

         !depositional fluxes, and diagensis fluxes
         drat=dep/max(dep,0.1_rk); rat=1.0_rk
         if(S2<=2.5_rk) rat=0.0_rk
         FS21=rat*drat*self%wss2*S2*self%fS21;   JS21=RS21*PS21
         FS22=rat*drat*self%wss2*S2*self%fS22;   JS22=RS22*PS22
         FDN1=drat*self%wsdn*DN*self%fDN1;   JDN1=RDN1*PDN1
         FDN2=drat*self%wsdn*DN*self%fDN2;   JDN2=RDN2*PDN2
         FDSi=drat*self%wsdsi*DSi*self%fDSi; JDSi=RDSi*PDSi

         !sediment nutrient fluxes
         nh4flx=JS21+JS22+JDN1+JDN2
         sio4flx=self%si2n*(JS21+JS22)+JDSi
         po4flx =self%p2n*(JS21+JS22+JDN1+JDN2)
         co2flx=self%c2n*nh4flx; o2flx=-self%o2nh*nh4flx
         if(self%ipo4==1) po4flx =po4flx+self%p2n*JDSi/self%si2n
      

         !reaction rates
         if(RS21<self.mkS21) then
           _ADD_BOTTOM_SOURCE_(self%id_RS21,(self%rkS21-RS21*FS21/max(mval,PS21))/secs_pr_day)
         endif
         if(RS22<self.mkS22) then
           _ADD_BOTTOM_SOURCE_(self%id_RS22,(self%rkS22-RS22*FS22/max(mval,PS22))/secs_pr_day)
         endif
         if(RDN1<self.mkDN1) then
           _ADD_BOTTOM_SOURCE_(self%id_RDN1,(self%rkDN1-RDN1*FDN1/max(mval,PDN1))/secs_pr_day)
         endif
         if(RDN2<self.mkDN2) then
           _ADD_BOTTOM_SOURCE_(self%id_RDN2,(self%rkDN2-RDN2*FDN2/max(mval,PDN2))/secs_pr_day)
         endif
         if(RDSi<self.mkDSi) then
           _ADD_BOTTOM_SOURCE_(self%id_RDSi,(self%rkDSi-RDSi*FDSi/max(mval,PDSi))/secs_pr_day)
         endif

         !sed conc.
         _ADD_BOTTOM_SOURCE_(self%id_PS21,(FS21-JS21)/secs_pr_day)
         _ADD_BOTTOM_SOURCE_(self%id_PS22,(FS22-JS22)/secs_pr_day)
         _ADD_BOTTOM_SOURCE_(self%id_PDN1,(FDN1-JDN1)/secs_pr_day)
         _ADD_BOTTOM_SOURCE_(self%id_PDN2,(FDN2-JDN2)/secs_pr_day)
         _ADD_BOTTOM_SOURCE_(self%id_PDSi,(FDSi-JDSi)/secs_pr_day)

         !sed flux
         _ADD_BOTTOM_FLUX_(self%id_SiO4,sio4flx/secs_pr_day)
         _ADD_BOTTOM_FLUX_(self%id_NH4, nh4flx/secs_pr_day)
         _ADD_BOTTOM_FLUX_(self%id_PO4, po4flx/secs_pr_day)
         _ADD_BOTTOM_FLUX_(self%id_DOX, o2flx/secs_pr_day)
         _ADD_BOTTOM_FLUX_(self%id_CO2, co2flx/secs_pr_day)
        
         _SET_BOTTOM_DIAGNOSTIC_(self%id_bTN,PS21+PS22+PDN1+PDN2)
      _BOTTOM_LOOP_END_
   end subroutine do_bottom

end module vims_cosine
