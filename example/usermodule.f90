  module usermodule
    implicit none

    ! Here we define a simple user type to be stored to the tree
    type mytype
      real :: a
    end type

    ! We need also a container to store the pointer value...
    type mytype_ptr
      type(mytype), pointer :: ptr
    end type mytype_ptr

  contains

    ! and a function able to compare our two nodes
    pure function userfun(a, b)
      use tree_m, only : DAT_KIND
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
