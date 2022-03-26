  module mytree
    use tree_m, only : rbtr_t, DAT_KIND
    use usermodule
    implicit none
    type, extends(rbtr_t) :: mytree_t
    contains
      procedure :: Printcurrentnode => mytree_Printcurrentnode
    end type mytree_t

  contains

    function mytree_Printcurrentnode(this, handle) result(str)
      class(mytree_t), intent(in) :: this
      integer(DAT_KIND), intent(in) :: handle(:)
      character(len=:), allocatable :: str
      character(len=1000) :: line

      type(mytype_ptr) :: dat
      integer :: ierr
      
      dat = transfer(this % Read(handle, ierr), dat)
      if (ierr /= 0) then
        str = 'current not associated...'
      else
        write(line,*) dat % ptr % a
        str = trim(adjustl(line))
      endif
    end function 


  end module mytree
