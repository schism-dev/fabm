module vims_model_library

   use fabm_types, only: type_base_model_factory, type_base_model

   implicit none

   private

   type, extends(type_base_model_factory) :: type_factory
   contains
      procedure :: create
   end type

   type (type_factory), save, target, public :: vims_model_factory

contains

   subroutine create(self, name, model)

      use vims_cosine ! Needs to be done for each vims model
      use vims_icm  

      class (type_factory), intent(in) :: self
      character(*),         intent(in) :: name
      class (type_base_model), pointer :: model

      select case (name)
         case ('cosine');   allocate(type_vims_cosine::model) ! Also add new models here
         case ('icm');      allocate(type_vims_icm::model) ! Also add new models here
      end select

   end subroutine

end module
