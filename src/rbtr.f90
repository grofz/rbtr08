  module rbtr_m
    use tree_common_m
    use basetree_m
    use kinds_m, only : I1B
    implicit none
    private

    integer, parameter :: RED_NODE = 1, BLACK_NODE = 0

    type, public, extends(basenode_t) :: rbnode_t
      private
      integer(I1B) :: color = RED_NODE
    contains
    end type rbnode_t



    type, public, extends(basetree_t) :: rbtr_t
    contains
      procedure :: Insert => rbtr_Insert
      procedure :: Isvalid_rbtree => rbtr_Isvalid_rbtree
      procedure :: Printcurrentnode
    end type rbtr_t

    interface rbtr_t
      module procedure rbtr_Initialize
    end interface rbtr_t



  contains

    function Printcurrentnode(this) result(str)
      character(len=:), allocatable :: str
      class(rbtr_t), intent(in) :: this
      character(len=1000) :: color
      if (.not. associated(this % current)) then
        str='current not associated...'
      else
        str = this % basetree_t % Printcurrentnode()
        select type(aa => this % current)
        class is (rbnode_t)
          write(color,*) aa % color
          str = str//' {'//trim(adjustl(color))//'}'
        end select
      endif
    end function


    function rbtr_Initialize() result(new)
      type(rbtr_t) :: new
!     allocate(rbnode_t :: new % typet)
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
      class(basenode_t), intent(in), pointer :: n
!
! Is node "n" black? Null node is always black.
!
      if (associated(n)) then
        select type(n)
        class is (rbnode_t)
          is_black = n % color == BLACK_NODE
        class default
          error stop "Is_black: node type must be extension of rbnode"
        end select
      else
        is_black = .true.
      endif
    end function Is_black



    function rbtr_Isvalid_rbtree(this) result(isvalid)
      logical :: isvalid
      class(rbtr_t), intent(in) :: this
!
! Verify that the properties of red-black tree are met
!
      integer :: blacks

      if (.not. associated(this % root)) then
        isvalid = .true.
        return
      endif

      select type(tree => this % root)
      class is (rbnode_t)
        call Verify_RB(tree, isvalid, blacks)
      class default
        error stop 'Isvalid_rbtree root must be rbnode_t'
      end select

      ! optional verification that the root is black
      ! TODO not needed

    end function rbtr_Isvalid_rbtree



    recursive subroutine Verify_RB(tree, isvalid, blacks)
      class(basenode_t), pointer, intent(in) :: tree
      logical, intent(out) :: isvalid
      integer, intent(out) :: blacks
!
! Recursivelt check all subtrees and assert red-black tree properties
!
      logical :: isvalid_left, isvalid_right
      integer :: blacks_left, blacks_right

      ! Empty tree is a valid tree (leaf nodes are black)
      if (.not. associated(tree)) then
        isvalid = .true.
        blacks = 1
        return
      endif

      ! Validate left and right sub-trees
      call Verify_RB(tree % Leftchild(), isvalid_left, blacks_left)

      call Verify_RB(tree % Rightchild(), isvalid_right, blacks_right)

      ! Validate that red node has only black childrens
      if (Is_black(tree)) then
        isvalid = .true.
      else
        isvalid = Is_black(tree % Leftchild()) .and. &
        &         Is_black(tree % Rightchild())
      endif

      ! Assert that number of black nodes is same in both sub-trees
      ! and return the number of blacks including the current node
      if (blacks_left /= blacks_right) isvalid = .false.

      blacks = blacks_left
      if (Is_black(tree)) blacks = blacks + 1
    end subroutine Verify_RB

  end module rbtr_m
