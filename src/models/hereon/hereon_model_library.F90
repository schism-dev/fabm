module hereon_model_library

   use fabm_types, only: type_base_model_factory, type_base_model

   implicit none

   private

   type, extends(type_base_model_factory) :: type_factory
   contains
      procedure :: create
   end type

   type (type_factory), save, target, public :: hereon_model_factory

contains

   subroutine create(self, name, model)

      use hereon_light
      use hereon_omexdia_p

      class (type_factory), intent(in) :: self
      character(*),         intent(in) :: name
      class (type_base_model), pointer :: model

      select case (name)
         case ('light');  allocate(type_hereon_light::model)
         case ('omexdia_p'); allocate(type_hereon_omexdia_p::model)
      end select

   end subroutine

end module
