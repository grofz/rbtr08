  module rbtr_m
    use tree_common_m
    use basetree_m
    use kinds_m, only : I1B
    implicit none
    private

    integer, parameter :: RED_NODE = 1, BLACK_NODE = 0

    type, extends(basenode_t), public :: rbnode_t
      private
      integer(I1B) :: color = RED_NODE
    contains
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
!       allocate(rbnode_t :: new % typet)
      end function rbtr_Initialize



      subroutine rbtr_Insert(this, dat, cfun, newnode, ierr)
        class(rbtr_t), intent(inout) :: this
        integer(DAT_KIND), intent(in) :: dat(:)
        procedure(cfun_abstract) :: cfun
        class(basenode_t), pointer, optional :: newnode
        integer, intent(out), optional :: ierr
!
! Insert a node to red-black tree
! Over-riding type-bound procedure in "basetree" class
!
        class(basenode_t), pointer :: new0
        integer :: ierr0

        if (present(newnode)) then
          new0 => newnode
 !error stop "rbtr_Insert: optional argument must not be present"
 print *, 'warning rbtr_insert: optional argument must not be present'
        else
          allocate(rbnode_t :: new0)
        endif
        select type(new0)
        class is (rbnode_t)
          new0 % color = RED_NODE
        class default
          error stop "rbtr_Insert: newnode must be an extension of rbnode_t"
        end select

        call this % basetree_t % Insert(dat, cfun, newnode=new0, ierr=ierr0)
  print *, 'must correct'

        if (associated(this % current, this % root)) then
                print *, 'new node is toor'
        else
                print *, 'new node is not root'
        endif

        if (present(ierr)) ierr = ierr0
      end subroutine rbtr_Insert



      logical function Is_black(n)
        class(rbnode_t), intent(in), pointer :: n
!
! Is node "n" black? Null node is always black.
!
        if (associated(n)) then
          is_black = n % color == BLACK_NODE
        else
          is_black = .true.
        endif
      end function Is_black
  end module rbtr_m
