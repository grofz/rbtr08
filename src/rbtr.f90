  module rbtr_m
    use tree_common_m
    use basetree_m
    implicit none
    private

    type, extends(basenode_t), public :: rbnode_t
      private
      logical :: black = .true.
    contains
      procedure :: Is_black
    end type rbnode_t



    type, extends(basetree_t), public :: rbtr_t
    contains
      procedure :: Insert => rbtr_Insert
    end type rbtr_t

    interface rbtr_t
      module procedure rbtr_Initialize
    end interface rbtr_t



  contains


      function rbtr_Initialize() result(new)
        type(rbtr_t) :: new
        allocate(rbnode_t :: new % typet)
      end function rbtr_Initialize



      function rbtr_Insert(a, dat, cfun) result(ierr)
        class(rbtr_t), intent(inout) :: a
        integer, intent(in) :: dat(:)
        procedure(cfun_abstract) :: cfun
        integer :: ierr

        ierr = a % basetree_t % Insert(dat, cfun)
        print *, 'must correct'
        
      end function rbtr_Insert


      logical function Is_black(n)
        class(rbnode_t), intent(in) :: n
        is_black = n % black
      end function Is_black
  end module rbtr_m
