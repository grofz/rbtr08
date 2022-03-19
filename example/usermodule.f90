
  module usermodule
    use tree_common_m, only : DAT_KIND
    implicit none

    ! Here we define a simple user type to be stored to the tree
    type mytype
      real :: a
    end type

    ! We need a container to store the pointer value,
    type mytype_ptr
      type(mytype), pointer :: ptr
    end type mytype_ptr

    ! a dummy integer array
    integer(DAT_KIND), parameter :: mold(1) = [0]

  contains

    ! and a function able to compare our two types
    pure function userfun(a, b)
      integer(DAT_KIND), intent(in) :: a(:), b(:)
      integer :: userfun
      type(mytype_ptr) :: adat, bdat

      adat = transfer(a, adat)
      bdat = transfer(b, adat)

      if (adat % ptr % a == bdat % ptr % a) then
        userfun = 0
      elseif (adat % ptr % a < bdat % ptr % a) then
        userfun = 1
      else
        userfun = -1
      endif
    end function userfun

  end module usermodule
