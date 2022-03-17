!
! Common utilities for all tree classes
!
  module tree_common_m
    use kinds_m, only : I4B
    implicit none
    public
    private I4B

    ! Kind of integer array containing the data
    integer, parameter :: DAT_KIND = I4B

    ! Function to compare two tree nodes
    abstract interface
      integer function cfun_abstract(a, b)
        import DAT_KIND
        integer(DAT_KIND), intent(in) :: a(:), b(:)
      end function
    end interface

    ! Error codes of tree processing routines
    integer, parameter :: TREE_ERR_OK = 0,         &
    &                     TREE_ERR_NOCURRENT = -1, &
    &                     TREE_ERR_EMPTY = -2,     &
    &                     TREE_ERR_NONEXT = 1

  end module tree_common_m
