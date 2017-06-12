#include "fabm_driver.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: Python interface to the Framework for Aquatic Biogeochemical Models (FABM)
!
! !INTERFACE:
   module fabm_python
!
! !DESCRIPTION:
! TODO
!
! !USES:
   use iso_c_binding, only: c_double, c_int, c_char, C_NULL_CHAR, c_f_pointer, c_loc, c_ptr

   !DIR$ ATTRIBUTES DLLEXPORT :: STATE_VARIABLE,DIAGNOSTIC_VARIABLE,CONSERVED_QUANTITY

   use fabm
   use fabm_config
   use fabm_types, only:rk,attribute_length,type_model_list_node,type_base_model, &
                        factory,type_link,type_link_list,type_internal_variable
   use fabm_driver, only: type_base_driver, driver
   use fabm_properties
   use fabm_python_helper
   use fabm_c_helper
   use fabm_c_model

   implicit none
!
! !PUBLIC MEMBER FUNCTIONS:
   public

   integer,parameter :: INTERIOR_STATE_VARIABLE        = 1
   integer,parameter :: SURFACE_STATE_VARIABLE         = 2
   integer,parameter :: BOTTOM_STATE_VARIABLE          = 3
   integer,parameter :: INTERIOR_DIAGNOSTIC_VARIABLE   = 4
   integer,parameter :: HORIZONTAL_DIAGNOSTIC_VARIABLE = 5
   integer,parameter :: CONSERVED_QUANTITY             = 6

   class (type_model),private,pointer,save :: model => null()
   character(len=1024),dimension(:),allocatable :: environment_names,environment_units
   integer :: index_column_depth
   real(c_double),pointer :: column_depth
   type (type_link_list),save :: coupling_link_list

   type,extends(type_base_driver) :: type_python_driver
   contains
      procedure :: fatal_error => python_driver_fatal_error
      procedure :: log_message => python_driver_log_message
   end type
!EOP
!-----------------------------------------------------------------------

   contains

   subroutine get_version(length,version_string) bind(c)
!DIR$ ATTRIBUTES DLLEXPORT :: get_version
      use fabm_version, only: fabm_commit_id=>git_commit_id, &
                              fabm_branch_name=>git_branch_name

      integer(c_int),value,intent(in) :: length
      character(kind=c_char)          :: version_string(length)

      call copy_to_c_string(fabm_commit_id//' ('//fabm_branch_name//' branch)', version_string)
   end subroutine get_version


!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialise the model
!
! !INTERFACE:
   subroutine initialize(path) bind(c)
!DIR$ ATTRIBUTES DLLEXPORT :: initialize
      character(kind=c_char),target,intent(in) :: path(*)
!
! !DESCRIPTION:
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!
      character(len=attribute_length),pointer :: ppath
      class (type_property),          pointer :: property
!-----------------------------------------------------------------------
!BOC
      ! Initialize driver object used by FABM for logging/error reporting.
      if (.not.associated(driver)) allocate(type_python_driver::driver)

      ! If the model object already exists, delete it to start from scratch.
      if (associated(model)) call finalize()

      ! Build FABM model tree (configuration will be read from file specified as argument).
      allocate(model)
      call c_f_pointer(c_loc(path), ppath)
      call fabm_create_model_from_yaml_file(model,path=ppath(:index(ppath,C_NULL_CHAR)-1))

      ! Send information on spatial domain to FABM (this also allocates memory for diagnostics)
      call fabm_set_domain(model)

      ! Retrieve arrays to hold values for environmental variables and corresponding metadata.
      call get_environment_metadata(model,environment_names,environment_units,index_column_depth)
      column_depth => null()

      call get_couplings(model,coupling_link_list)
   end subroutine initialize
!EOC

   function variable_get_suitable_masters(pvariable) result(plist) bind(c)
      !DIR$ ATTRIBUTES DLLEXPORT :: variable_get_suitable_masters
      type (c_ptr), intent(in), value :: pvariable
      type (c_ptr)                    :: plist

      type (type_internal_variable),pointer :: variable
      type (type_link_list),        pointer :: list

      call c_f_pointer(pvariable, variable)
      list => get_suitable_masters(model,variable)
      plist = c_loc(list)
   end function variable_get_suitable_masters

   subroutine reinitialize(name, value)
      character(len=*),      intent(in)           :: name
      class (type_property), intent(in), optional :: value

      type (type_model),           pointer :: newmodel
      type (type_model_list_node), pointer :: node
      class (type_base_model),     pointer :: childmodel
      class (type_property),       pointer :: property,next

      ! Create new model object.
      allocate(newmodel)

      ! Transfer previous parameters to root of the model.
      call newmodel%root%parameters%update(model%root%parameters, minimum_source=property_source_user)
      call newmodel%root%couplings%update(model%root%couplings, minimum_source=property_source_user)

      if (.not.present(value)) then
         call newmodel%root%parameters%delete(name)
      else
         call newmodel%root%parameters%set(name, value)
      end if

      ! Re-create original models
      node => model%root%children%first
      do while (associated(node))
         if (node%model%user_created) then
            call factory%create(node%model%type_name,childmodel)
            childmodel%user_created = .true.
            call newmodel%root%add_child(childmodel,node%model%name,node%model%long_name,configunit=-1)
         end if
         node => node%next
      end do

      ! Clean up old model
      call finalize()
      model => newmodel

      ! Initialize new model
      call fabm_initialize(model)

      ! Removed unused forced parameters from root model.
      !property => model%root%parameters%first
      !do while (associated(property))
      !   if (.not. model%root%parameters%retrieved%contains(property%name)) then
      !      next => property%next
      !      call model%root%parameters%delete(property%name)
      !      property => next
      !   else
      !      property => property%next
      !   end if
      !end do

      ! Send information on spatial domain to FABM (this also allocates memory for diagnostics)
      call fabm_set_domain(model)

      ! Retrieve arrays to hold values for environmental variables and corresponding metadata.
      call get_environment_metadata(model,environment_names,environment_units,index_column_depth)
      column_depth => null()

      call get_couplings(model,coupling_link_list)
   end subroutine reinitialize

   subroutine check_ready() bind(c)
      !DIR$ ATTRIBUTES DLLEXPORT :: check_ready
      call fabm_check_ready(model)
   end subroutine check_ready

   subroutine get_counts(nstate_interior,nstate_surface,nstate_bottom,ndiagnostic_interior,ndiagnostic_horizontal,nconserved, &
      ndependencies) bind(c)
      !DIR$ ATTRIBUTES DLLEXPORT :: get_counts
      integer(c_int),intent(out) :: nstate_interior,nstate_surface,nstate_bottom
      integer(c_int),intent(out) :: ndiagnostic_interior,ndiagnostic_horizontal
      integer(c_int),intent(out) :: nconserved,ndependencies
      nstate_interior = size(model%state_variables)
      nstate_surface = size(model%surface_state_variables)
      nstate_bottom = size(model%bottom_state_variables)
      ndiagnostic_interior = size(model%diagnostic_variables)
      ndiagnostic_horizontal = size(model%horizontal_diagnostic_variables)
      nconserved = size(model%conserved_quantities)
      ndependencies = size(environment_names)
   end subroutine get_counts

   function get_root() bind(c) result(proot)
      !DIR$ ATTRIBUTES DLLEXPORT :: get_root
      type (c_ptr) :: proot

      type (type_model_list_node),pointer :: res

      allocate(res)
      res%model => model%root
      proot = c_loc(res)
   end function get_root

   subroutine get_variable_metadata(category,index,length,name,units,long_name,path) bind(c)
      !DIR$ ATTRIBUTES DLLEXPORT :: get_variable_metadata
      integer(c_int),        intent(in), value             :: category,index,length
      character(kind=c_char),intent(out),dimension(length) :: name,units,long_name,path

      class (type_external_variable),pointer :: variable

      ! Get a pointer to the target variable
      select case (category)
      case (INTERIOR_STATE_VARIABLE)
         variable => model%state_variables(index)
      case (SURFACE_STATE_VARIABLE)
         variable => model%surface_state_variables(index)
      case (BOTTOM_STATE_VARIABLE)
         variable => model%bottom_state_variables(index)
      case (INTERIOR_DIAGNOSTIC_VARIABLE)
         variable => model%diagnostic_variables(index)
      case (HORIZONTAL_DIAGNOSTIC_VARIABLE)
         variable => model%horizontal_diagnostic_variables(index)
      case (CONSERVED_QUANTITY)
         variable => model%conserved_quantities(index)
      end select
      call copy_to_c_string(variable%name,           name)
      call copy_to_c_string(variable%units,          units)
      call copy_to_c_string(variable%local_long_name,long_name)
      call copy_to_c_string(variable%path,           path)
   end subroutine get_variable_metadata

   function get_variable(category,index) bind(c) result(pvariable)
      !DIR$ ATTRIBUTES DLLEXPORT :: get_variable
      integer(c_int),intent(in), value :: category,index
      type (c_ptr)                     :: pvariable

      type (type_internal_variable),pointer :: variable

      ! Get a pointer to the target variable
      select case (category)
      case (INTERIOR_STATE_VARIABLE)
         variable => model%state_variables(index)%target
      case (SURFACE_STATE_VARIABLE)
         variable => model%surface_state_variables(index)%target
      case (BOTTOM_STATE_VARIABLE)
         variable => model%bottom_state_variables(index)%target
      case (INTERIOR_DIAGNOSTIC_VARIABLE)
         variable => model%diagnostic_variables(index)%target
      case (HORIZONTAL_DIAGNOSTIC_VARIABLE)
         variable => model%horizontal_diagnostic_variables(index)%target
      case (CONSERVED_QUANTITY)
         variable => model%conserved_quantities(index)%target
      end select
      pvariable = c_loc(variable)
   end function get_variable

   subroutine get_dependency_metadata(index,length,name,units) bind(c)
      !DIR$ ATTRIBUTES DLLEXPORT :: get_dependency_metadata
      integer(c_int),        intent(in), value             :: index,length
      character(kind=c_char),intent(out),dimension(length) :: name,units

      call copy_to_c_string(environment_names(index),name)
      call copy_to_c_string(environment_units(index),units)
   end subroutine get_dependency_metadata

   subroutine get_coupling(index,slave,master) bind(c)
      !DIR$ ATTRIBUTES DLLEXPORT :: get_coupling
      integer(c_int),        intent(in), value :: index
      type (c_ptr),          intent(out)       :: slave,master

      type (type_link),pointer :: link_slave
      integer                  :: i

      link_slave => coupling_link_list%first
      do i=2,index
         link_slave => link_slave%next
      end do
      slave = c_loc(link_slave%original)
      master = c_loc(link_slave%target)
   end subroutine get_coupling

   subroutine link_dependency_data(index,value) bind(c)
      !DIR$ ATTRIBUTES DLLEXPORT :: link_dependency_data
      integer(c_int),intent(in),value  :: index
      real(c_double),intent(in),target :: value

      call fabm_link_interior_data(model,environment_names(index),value)
      call fabm_link_horizontal_data(model,environment_names(index),value)
      call fabm_link_scalar_data(model,environment_names(index),value)
      if (index==index_column_depth) column_depth => value
   end subroutine link_dependency_data

   subroutine link_interior_state_data(index,value) bind(c)
      !DIR$ ATTRIBUTES DLLEXPORT :: link_interior_state_data
      integer(c_int),intent(in),   value  :: index
      real(c_double),intent(inout),target :: value

      value = model%state_variables(index)%initial_value
      call fabm_link_interior_state_data(model,index,value)
   end subroutine link_interior_state_data

   subroutine link_surface_state_data(index,value) bind(c)
      !DIR$ ATTRIBUTES DLLEXPORT :: link_surface_state_data
      integer(c_int),intent(in),   value  :: index
      real(c_double),intent(inout),target :: value

      value = model%surface_state_variables(index)%initial_value
      call fabm_link_surface_state_data(model,index,value)
   end subroutine link_surface_state_data

   subroutine link_bottom_state_data(index,value) bind(c)
      !DIR$ ATTRIBUTES DLLEXPORT :: link_bottom_state_data
      integer(c_int),intent(in),   value  :: index
      real(c_double),intent(inout),target :: value

      value = model%bottom_state_variables(index)%initial_value
      call fabm_link_bottom_state_data(model,index,value)
   end subroutine link_bottom_state_data

   subroutine get_rates(pelagic_rates_, do_surface, do_bottom) bind(c)
      !DIR$ ATTRIBUTES DLLEXPORT :: get_rates
      real(c_double),target,intent(in) :: pelagic_rates_(*)
      integer(c_int),value, intent(in) :: do_surface, do_bottom

      real(c_double),pointer :: pelagic_rates(:)
      real(rk)               :: ext

      call fabm_get_light_extinction(model,ext)
      call fabm_get_light(model)
      call c_f_pointer(c_loc(pelagic_rates_),pelagic_rates, &
        (/size(model%state_variables)+size(model%surface_state_variables)+size(model%bottom_state_variables)/))
      pelagic_rates = 0.0_rk
      if (int2logical(do_surface)) call fabm_do_surface(model,pelagic_rates(1:size(model%state_variables)), &
         pelagic_rates(size(model%state_variables)+1:size(model%state_variables)+size(model%surface_state_variables)))
      if (int2logical(do_bottom)) call fabm_do_bottom(model,pelagic_rates(1:size(model%state_variables)), &
         pelagic_rates(size(model%state_variables)+size(model%surface_state_variables)+1:))
      if (int2logical(do_surface) .or. int2logical(do_bottom)) then
         if (.not.associated(column_depth)) call driver%fatal_error('get_rates', &
            'Value for environmental dependency '//trim(environment_names(index_column_depth))// &
            ' must be provided if get_rates is called with the do_surface and/or do_bottom flags.')
         pelagic_rates(1:size(model%state_variables)) = pelagic_rates(1:size(model%state_variables))/column_depth
      end if
      call fabm_do(model,pelagic_rates(1:size(model%state_variables)))

      ! Compute rate of change in conserved quantities
      !call fabm_state_to_conserved_quantities(model,pelagic_rates,conserved_rates)

      ! Normalize rate of change in conserved quantities to sum of absolute rates of change.
      !call fabm_state_to_conserved_quantities(model,abs(pelagic_rates),abs_conserved_rates)
      !where (abs_conserved_rates>0.0_rk) conserved_rates = conserved_rates/abs_conserved_rates
   end subroutine get_rates

   subroutine get_interior_diagnostic_data(index,ptr) bind(c)
      !DIR$ ATTRIBUTES DLLEXPORT :: get_interior_diagnostic_data
      integer(c_int),intent(in),value :: index
      type(c_ptr),   intent(out)      :: ptr
      ptr = c_loc(fabm_get_interior_diagnostic_data(model,index))
   end subroutine get_interior_diagnostic_data

   subroutine get_horizontal_diagnostic_data(index,ptr) bind(c)
      !DIR$ ATTRIBUTES DLLEXPORT :: get_horizontal_diagnostic_data
      integer(c_int),intent(in),value :: index
      type(c_ptr),   intent(out)      :: ptr
      ptr = c_loc(fabm_get_horizontal_diagnostic_data(model,index))
   end subroutine get_horizontal_diagnostic_data

   subroutine finalize()
      call fabm_finalize(model)
      if (allocated(environment_names)) deallocate(environment_names)
      if (allocated(environment_units)) deallocate(environment_units)
   end subroutine finalize

   subroutine reset_parameter(name) bind(c)
      !DIR$ ATTRIBUTES DLLEXPORT :: reset_parameter
      character(kind=c_char),target,intent(in) :: name(*)

      character(len=attribute_length),pointer :: pname

      call c_f_pointer(c_loc(name), pname)
      call reinitialize(pname(:index(pname,C_NULL_CHAR)-1))
   end subroutine reset_parameter

   subroutine set_real_parameter(name,value) bind(c)
      !DIR$ ATTRIBUTES DLLEXPORT :: set_real_parameter
      character(kind=c_char),target,intent(in) :: name(*)
      real(c_double),value,         intent(in) :: value

      character(len=attribute_length),pointer :: pname

      call c_f_pointer(c_loc(name), pname)
      call reinitialize(pname(:index(pname,C_NULL_CHAR)-1),type_real_property(value=value,source=property_source_user))
   end subroutine set_real_parameter

   subroutine set_integer_parameter(name,value) bind(c)
      !DIR$ ATTRIBUTES DLLEXPORT :: set_integer_parameter
      character(kind=c_char),target,intent(in) :: name(*)
      integer(c_int),value,         intent(in) :: value

      character(len=attribute_length),pointer :: pname

      call c_f_pointer(c_loc(name), pname)
      call reinitialize(pname(:index(pname,C_NULL_CHAR)-1),type_integer_property(value=value,source=property_source_user))
   end subroutine set_integer_parameter

   subroutine set_logical_parameter(name,value) bind(c)
      !DIR$ ATTRIBUTES DLLEXPORT :: set_logical_parameter
      character(kind=c_char),target,    intent(in) :: name(*)
      integer(c_int),value,intent(in) :: value

      character(len=attribute_length),pointer :: pname

      call c_f_pointer(c_loc(name), pname)
      call reinitialize(pname(:index(pname,C_NULL_CHAR)-1),type_logical_property(value=int2logical(value),source=property_source_user))
   end subroutine set_logical_parameter

   subroutine set_string_parameter(name,value) bind(c)
      !DIR$ ATTRIBUTES DLLEXPORT :: set_string_parameter
      character(kind=c_char),target,intent(in) :: name(*),value(*)

      character(len=attribute_length),pointer :: pname,pvalue

      call c_f_pointer(c_loc(name),  pname)
      call c_f_pointer(c_loc(value), pvalue)
      call reinitialize(pname(:index(pname,C_NULL_CHAR)-1),type_string_property(value=pvalue(:index(pname,C_NULL_CHAR)-1),source=property_source_user))
   end subroutine set_string_parameter

   subroutine python_driver_fatal_error(self,location,message)
      class (type_python_driver),intent(inout) :: self
      character(len=*),          intent(in)    :: location,message

      write (*,*) trim(location)//': '//trim(message)
      stop 1
   end subroutine python_driver_fatal_error

   subroutine python_driver_log_message(self,message)
      class (type_python_driver),intent(inout) :: self
      character(len=*),          intent(in)    :: message

      !write (*,*) trim(message)
   end subroutine python_driver_log_message

   end module fabm_python

!-----------------------------------------------------------------------
! Copyright Bolding & Bruggeman ApS - Public License - www.gnu.org
!-----------------------------------------------------------------------
