module fabm_c_parameter

   use iso_c_binding, only: c_double, c_int, c_char, C_NULL_CHAR, c_f_pointer, c_loc, c_ptr, C_NULL_PTR

   use fabm_properties
   use fabm_c_helper
   use fabm_driver
   
   implicit none
   
   public

contains

   function get_first_used_parameter(pair) result(pair_retrieved)
      type (type_key_property_pair),pointer :: pair, pair_retrieved
      pair_retrieved => pair
      do while (associated(pair_retrieved))
         if (pair_retrieved%retrieved) return
         pair_retrieved => pair_retrieved%next
      end do
   end function

   function parameter_get_next(ppair) bind(c) result(pnext)
      !DIR$ ATTRIBUTES DLLEXPORT :: parameter_get_next
      type (c_ptr), intent(in), value :: ppair
      type (c_ptr)                    :: pnext

      type (type_key_property_pair),pointer :: pair

      pnext = C_NULL_PTR
      call c_f_pointer(ppair, pair)
      pair => get_first_used_parameter(pair%next)
      if (associated(pair)) pnext = c_loc(pair)
   end function parameter_get_next

   subroutine parameter_get_metadata(ppair,length,name,units,long_name,typecode,has_default) bind(c)
      !DIR$ ATTRIBUTES DLLEXPORT :: parameter_get_metadata
      type (c_ptr),          intent(in), value             :: ppair
      integer(c_int),        intent(in), value             :: length
      character(kind=c_char),intent(out),dimension(length) :: name,units,long_name
      integer(c_int),        intent(out)                   :: typecode,has_default

      type (type_key_property_pair),pointer :: pair

      call c_f_pointer(ppair, pair)
      
      call copy_to_c_string(pair%name,              name)
      call copy_to_c_string(pair%property%units,    units)
      call copy_to_c_string(pair%property%long_name,long_name)
      typecode = pair%property%typecode()
      has_default = logical2int(pair%property%has_default)
   end subroutine parameter_get_metadata

   function parameter_get_real_value(ppair,default) bind(c) result(value)
      !DIR$ ATTRIBUTES DLLEXPORT :: parameter_get_real_value
      type (c_ptr),  value, intent(in) :: ppair
      integer(c_int),value, intent(in) :: default
      real(c_double)                   :: value
      class (type_property),pointer    :: property

      type (type_key_property_pair),pointer :: pair

      call c_f_pointer(ppair, pair)

      select type (property=>pair%property)
      class is (type_real_property)
         if (int2logical(default)) then
            value = property%default
         else
            value = property%value
         end if
      class default
         call driver%fatal_error('parameter_get_real_value',trim(pair%name)//': not a real variable')
      end select
   end function parameter_get_real_value

   function parameter_get_integer_value(ppair,default) bind(c) result(value)
      !DIR$ ATTRIBUTES DLLEXPORT :: parameter_get_integer_value
      type (c_ptr),  value,intent(in) :: ppair
      integer(c_int),value,intent(in) :: default
      integer(c_int)                  :: value
      class (type_property),pointer   :: property

      type (type_key_property_pair),pointer :: pair

      call c_f_pointer(ppair, pair)

      select type (property=>pair%property)
      class is (type_integer_property)
         if (int2logical(default)) then
            value = property%default
         else
            value = property%value
         end if
      class default
         call driver%fatal_error('parameter_get_integer_value',trim(pair%name)//': not an integer variable')
      end select
   end function parameter_get_integer_value

   function parameter_get_logical_value(ppair,default) bind(c) result(value)
      !DIR$ ATTRIBUTES DLLEXPORT :: parameter_get_logical_value
      type (c_ptr),  value,intent(in) :: ppair
      integer(c_int),value,intent(in) :: default
      integer(c_int)                  :: value
      class (type_property),pointer   :: property

      type (type_key_property_pair),pointer :: pair

      call c_f_pointer(ppair, pair)

      select type (property=>pair%property)
      class is (type_logical_property)
         if (int2logical(default)) then
            value = logical2int(property%default)
         else
            value = logical2int(property%value)
         end if
      class default
         call driver%fatal_error('parameter_get_logical_value',trim(pair%name)//': not a logical variable')
      end select
   end function parameter_get_logical_value

   subroutine parameter_get_string_value(ppair,default,length,value) bind(c)
      !DIR$ ATTRIBUTES DLLEXPORT :: parameter_get_string_value
      type (c_ptr),          intent(in), value             :: ppair
      integer(c_int),value,intent(in) :: default,length
      character(kind=c_char)          :: value(length)

      type (type_key_property_pair),pointer :: pair

      call c_f_pointer(ppair, pair)

      select type (property=>pair%property)
      class is (type_string_property)
         if (int2logical(default)) then
            call copy_to_c_string(property%default, value)
         else
            call copy_to_c_string(property%value, value)
         end if
      class default
         call driver%fatal_error('parameter_get_string_value',trim(pair%name)//': not a string variable')
      end select
   end subroutine parameter_get_string_value

end module fabm_c_parameter