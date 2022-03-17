  module basetree_m
    use tree_common_m
    implicit none
    private

    type, public :: basenode_t
      private
      class(basenode_t), pointer :: parent => null()
      class(basenode_t), pointer :: left => null()
      class(basenode_t), pointer :: right => null()
      integer(DAT_KIND), allocatable :: dat(:)
    contains
      procedure :: Parentf
      procedure :: Grandparent
      procedure :: Uncle
      final :: Finalize_basenode
    end type basenode_t



    type, public :: basetree_t
      private
      class(basenode_t), pointer :: root => null()
      class(basenode_t), pointer :: current => null()
      class(basenode_t), allocatable, public :: typet
      integer :: nodes = 0
    contains
      procedure :: Insert => basetree_Insert
    end type basetree_t

    interface basetree_t
      module procedure basetree_Initialize
    end interface basetree_t


    integer, parameter :: ERR_NOTINIT = -5, ERR_OK = 0


  contains


    function basetree_Initialize() result(new)
      type(basetree_t) :: new
      allocate(basenode_t :: new % typet)
    end function basetree_Initialize



    function basetree_Insert(a, dat, cfun) result(ierr)
      class(basetree_t), intent(inout) :: a
      integer(DAT_KIND), intent(in) :: dat(:)
      procedure(cfun_abstract) :: cfun
      integer :: ierr
!
! Insert new node to the tree.
!        
      class(basenode_t), pointer :: new

      ! Prepare new node to be selected to tree
      if (.not. allocated(a % typet)) then
        ierr = ERR_NOTINIT
        return
      endif
      allocate(new, mold = a % typet)
      new % dat = dat

      a % nodes = a % nodes + 1
      a % current => new
      ierr = ERR_OK

      ! If it is a first node 
      if (.not. associated(a % root)) then
        a % root => new
        if (a % nodes /= 1) &
        &   error stop 'basetree_Insert ERROR: nodes /= 1'
        return
      endif

      ! Insert new node into the current tree
      call insert_recurse(a, a % root, new, cfun)

    end function basetree_Insert



    recursive subroutine insert_recurse(a, root, new, cfun)
      class(basetree_t), intent(inout) :: a
      class(basenode_t), intent(inout), pointer :: root, new
      procedure(cfun_abstract) :: cfun

      integer :: ires

      if (.not. associated(root)) then
        error stop "insert_recurse: something got wrong"
      endif

      ires = cfun(new % dat, root % dat)
      select case(ires)
      case(+1)
        ! Insert node to the left-subtree
        if (associated(root % left)) then
          call insert_recurse(a, root % left, new, cfun)
          return
        else
          root % left => new
          new % parent => root
        endif

      case(-1)
        ! Insert node to the right-subtree
        if (associated(root % right)) then
          call insert_recurse(a, root % right, new, cfun)
          return
        else
          root % left => new
          new % parent => root
        endif

      case(0)
        ! Duplicit nodes
      case default
        error stop "insert_recurse: invalid result from cfun"
      end select
    end subroutine insert_recurse



    function Parentf(n)
      class(basenode_t), pointer :: parentf
      class(basenode_t), intent(in) :: n

      if (associated(n % parent)) then
        parentf => n % parent
      else
        parentf => null()
      endif
    end function Parentf  



    function Grandparent(n)
      class(basenode_t), pointer :: grandparent
      class(basenode_t), intent(in) :: n

      class(basenode_t), pointer :: p

      p => n % Parentf()
      if (associated(p)) then
        grandparent => p % Parentf()
      else
        grandparent => null()
      endif
    end function Grandparent



    function Sibling(n)
      class(basenode_t), pointer :: sibling
      class(basenode_t), intent(in), pointer :: n

      class(basenode_t), pointer :: p

      p => n % Parentf()
      if (associated(p)) then
        if (associated(n, p % left)) then
          sibling => p % right
        elseif (associated(n, p % right)) then
          sibling => p % left
        else
          error stop 'basetree::Sibling ERROR. n is uknown to p'
        endif
      else
        sibling => null()
      endif
    end function Sibling



    function Uncle(n)
      class(basenode_t), pointer :: uncle
      class(basenode_t), intent(in) :: n

      class(basenode_t), pointer :: p

      p => n % Parentf()
      if (associated(p)) then
        uncle => Sibling(p)
      else
        uncle => null()
      endif
    end function Uncle



    logical function Is_left_child(n)
      class(basenode_t), intent(in), pointer :: n

      class(basenode_t), pointer :: p

      p => n % Parentf()
      if (associated(p)) then
        if (associated(n, p % left)) then
          is_left_child = .true.
        elseif (associated(n, p % right)) then
          is_left_child = .false.
        else
          error stop 'basetree::Is_left_child ERROR. n is uknown to p'
        endif
      else
        ! node has no parent
        is_left_child = .false.
      endif
    end function Is_left_child



    logical function Is_right_child(n)
      class(basenode_t), intent(in), pointer :: n

      class(basenode_t), pointer :: p

      p => n % Parentf()
      if (associated(p)) then
        is_right_child = .not. Is_left_child(n)
      else
        ! node has no parent
        is_right_child = .false.
      endif
    end function Is_right_child



    logical function Is_inner_grandparent(n)
      class(basenode_t), intent(in), pointer :: n

      class(basenode_t), pointer :: p

      is_inner_grandparent = .false.
      p => n % Parentf()
      if (.not. associated(p)) return

      if (Is_left_child(n)) then
        if (Is_right_child(p)) is_inner_grandparent = .true.
      elseif (Is_right_child(n)) then
        if (Is_left_child(p)) is_inner_grandparent = .true.
      endif
    end function Is_inner_grandparent



    subroutine Finalize_basenode(n)
      type(basenode_t), intent(inout) :: n
      if (allocated(n % dat)) deallocate(n % dat)
    end subroutine Finalize_basenode
    

  end module basetree_m
