module fabm_c_model

   use iso_c_binding, only: c_double, c_int, c_char, C_NULL_CHAR, c_f_pointer, c_loc, c_ptr, C_NULL_PTR
   use fabm_types
   use fabm_driver
   use fabm_c_helper
   use fabm_properties
   use fabm_c_parameter

   implicit none
   
   public

contains

   function model_get_first_parameter(pmodel) bind(c) result(ppair)
      !DIR$ ATTRIBUTES DLLEXPORT :: model_get_first_parameter
      type (c_ptr), intent(in), value :: pmodel
      type (c_ptr)                    :: ppair

      type (type_model_list_node),  pointer :: node
      type (type_key_property_pair),pointer :: pair

      ppair = C_NULL_PTR
      call c_f_pointer(pmodel, node)
      pair => get_first_used_parameter(node%model%parameters%first)
      if (associated(pair)) ppair = c_loc(pair)
   end function model_get_first_parameter

   function model_get_first_child(pmodel) bind(c) result(pchild)
      !DIR$ ATTRIBUTES DLLEXPORT :: model_get_first_child
      type (c_ptr),   intent(in), value :: pmodel
      type (c_ptr)                      :: pchild

      type (type_model_list_node), pointer :: node

      pchild = C_NULL_PTR
      call c_f_pointer(pmodel, node)
      if (associated(node%model%children%first)) pchild = c_loc(node%model%children%first)
   end function model_get_first_child

   function model_get_next(pmodel) bind(c) result(pnext)
      !DIR$ ATTRIBUTES DLLEXPORT :: model_get_next
      type (c_ptr),   intent(in), value :: pmodel
      type (c_ptr)                      :: pnext

      type (type_model_list_node), pointer :: node

      pnext = C_NULL_PTR
      call c_f_pointer(pmodel, node)
      if (associated(node%next)) pnext = c_loc(node%next)
   end function model_get_next

   function model_find_child(pmodel,name) bind(c) result(pchild)
      !DIR$ ATTRIBUTES DLLEXPORT :: model_find_child
      type (c_ptr),          intent(in), value  :: pmodel
      character(kind=c_char),intent(in), target :: name(*)
      type (c_ptr)                              :: pchild

      type (type_model_list_node),    pointer :: node
      character(len=attribute_length),pointer :: pname
      class (type_base_model),        pointer :: found_model
      type (type_model_list_node),    pointer :: res

      call c_f_pointer(pmodel, node)
      call c_f_pointer(c_loc(name), pname)
      found_model => node%model%find_model(pname(:index(pname,C_NULL_CHAR)-1))
      if (.not.associated(found_model)) then
         pchild = C_NULL_PTR
         return
      end if
      allocate(res)
      res%model => found_model
      pchild = c_loc(res)
   end function model_find_child

   subroutine model_get_metadata(pmodel,length,name,long_name,user_created) bind(c)
      !DIR$ ATTRIBUTES DLLEXPORT :: model_get_metadata
      type (c_ptr),          intent(in), value  :: pmodel
      integer(c_int),        intent(in), value  :: length
      character(kind=c_char),intent(out)        :: name(length),long_name(length)
      integer(c_int),        intent(out)        :: user_created

      type (type_model_list_node), pointer :: node

      call c_f_pointer(pmodel, node)
      call copy_to_c_string(node%model%long_name,long_name)
      call copy_to_c_string(node%model%name,name)
      user_created = logical2int(node%model%user_created)
   end subroutine model_get_metadata

   subroutine model_get_path(pmodel, length, path) bind(c)
      !DIR$ ATTRIBUTES DLLEXPORT :: model_get_path
      type (c_ptr),          intent(in), value  :: pmodel
      integer(c_int),        intent(in), value  :: length
      character(kind=c_char),intent(out)        :: path(length)

      type (type_model_list_node), pointer :: node
      character(len=attribute_length) :: path_

      call c_f_pointer(pmodel, node)
      path_ = node%model%get_path()
      call copy_to_c_string(path_,path)
   end subroutine model_get_path

end module fabm_c_model