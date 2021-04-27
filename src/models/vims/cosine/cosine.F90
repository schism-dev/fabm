#include "fabm_driver.h"

! This is a placeholder for the cosine model only !!

! CoSiNE stands for Carbon, Silicate, Nitrogen Ecosystem, which was originally
! developed by Prof. Fei Chai (U. of Maine) for modeling the ocean biogeochemical
! processes for the equatorial Pacific and the Pacific Ocean (Chai, Dugdale et al.
! 2002, Chai, Jiang et al. 2003, Chai, Jiang et al. 2007). In CoSiNE model, there
! are 13 state variables including 2 phytoplankton species (S1 and S2), 2
! zooplankton species (Z1 and Z2), 3 nitrogen forms (NO3, NH4 and DN (detritus
! nitrogen)), 2 silicon forms (SiO4 and DSi (detritus silicon)) and 1 phosphorus
! form (PO4), dissolved Oxygen (DOX), total carbon dioxide (CO2) and total
! alkalinity (ALK). T
!
! Original author(s): Fei Chai (U Maine)
!
! @author  => add authors
! @copyright  => add VIMS etc ...
! @license => speak to Fei Chai, or is there a license for Cosine already?

module vims_cosine

   use fabm_types

   implicit none

   private

   type, extends(type_base_model), public :: type_vims_cosine
      ! Variable identifiers
      type (type_state_variable_id)      :: id_no3, id_sio4, id_nh4
      type (type_state_variable_id)      :: id_s1, id_s2, id_z1, id_z2
      type (type_state_variable_id)      :: id_dn, id_dsi, id_po4
      type (type_state_variable_id)      :: id_dox, id_co2, id_alk

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


      ! Store parameter values in our own derived type
      ! NB: all rates must be provided in values per day in te configuration file,
      ! and are converted here to values per second by specifying scale_factor=d_per_s.
      ! call self%get_parameter(self%p0,'p0','mmol m-3','background phytoplankton concentration ',default=0.0225_rk)

      ! Register state variables, this must be done in this specific order, as
      ! SCHISM relies on this ordering
      ! Nitrate NO3 1 mmol/m3
      ! Silicate SiO4 2 mmol/m3
      ! Ammonium NH4 3 mmol/m3
      ! Small Phytoplankton S1 4 mmol/m3
      ! Diatom S2 5 mmol/m3
      ! Microzooplankton Z1 6 mmol/m3
      ! Mesozooplankton Z2 7 mmol/m3
      ! Detritus Nitrogen DN 8 mmol/m3
      ! Detritus Silicon DSi 9 mmol/m3
      ! Phosphate PO4 10 mmol/m3
      ! Dissolved Oxygen DOX 11 mmol m-3
      ! Dioxide Carbon CO2 12 mmol m-3
      ! Alkalinity ALK

      call self%register_state_variable(self%id_no3, 'no3', 'mmol m-3', 'Nitrate',    4.5_rk, minimum=0.0_rk, no_river_dilution=.true.)

      ! Register the contribution of all state variables to total nitrogen
      call self%add_to_aggregate_variable(standard_variables%total_nitrogen, self%id_no3)

      ! Register environmental dependencies
      ! call self%register_dependency(self%id_par, standard_variables%downwelling_photosynthetic_radiative_flux)
      ! call self%register_dependency(self%id_I_0, standard_variables%surface_downwelling_photosynthetic_radiative_flux)

      ! Let phytoplankton (including background concentration) and detritus contribute to light attentuation
      ! call self%add_to_aggregate_variable(standard_variables%attenuation_coefficient_of_photosynthetic_radiative_flux, self%id_p, scale_factor=self%kc)

   end subroutine initialize

   subroutine do(self, _ARGUMENTS_DO_)
      class (type_vims_cosine), intent(in) :: self
      _DECLARE_ARGUMENTS_DO_

      real(rk)            :: no3
      real(rk), parameter :: secs_pr_day = 86400.0_rk

      ! Enter spatial loops (if any)
      _LOOP_BEGIN_

         ! Retrieve current (local) state variable values.
         _GET_(self%id_no3, no3)

         ! Retrieve current environmental conditions.
         ! _GET_(self%id_par,par)          ! local photosynthetically active radiation
         ! _GET_SURFACE_(self%id_I_0,I_0)  ! surface photosynthetically active radiation

         ! Set temporal derivatives
         _ADD_SOURCE_(self%id_no3, 0)

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
         _GET_(self%id_no3, no3) ! nutrient

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
