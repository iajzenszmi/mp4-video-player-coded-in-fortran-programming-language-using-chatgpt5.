# mp4-video-player-coded-in-fortran-programming-language-using-chatgpt5.
'/home/ian/Downloads/Music from Carmen _ 1984 Schools Spectacular.mp4'
Skip to content
Chat history
You said:
mp4 player in fortran
ChatGPT said:

Here’s a practical way to play an MP4 from Fortran using the libmpv (mpv) C API via ISO_C_BINDING. It’s lightweight and works well on Debian/Ubuntu.
1) Install dependencies (Debian/Ubuntu)

sudo apt update
sudo apt install mpv libmpv-dev gfortran

2) Fortran source (mp4_player_mpv.f90)

! mp4_player_mpv.f90
! Minimal MP4 player using libmpv via ISO_C_BINDING
! Build: gfortran mp4_player_mpv.f90 -o fmpv -lmpv
! Usage:  ./fmpv path/to/video.mp4

program mp4_player_mpv
  use iso_c_binding
  implicit none

  !---- libmpv C types & interfaces -----------------------------------------
  integer(c_int), parameter :: MPV_FORMAT_NONE = 0
  integer(c_int), parameter :: MPV_EVENT_NONE = 0
  integer(c_int), parameter :: MPV_EVENT_SHUTDOWN = 1
  integer(c_int), parameter :: MPV_EVENT_LOG_MESSAGE = 2
  integer(c_int), parameter :: MPV_EVENT_GET_PROPERTY_REPLY = 3
  integer(c_int), parameter :: MPV_EVENT_SET_PROPERTY_REPLY = 4
  integer(c_int), parameter :: MPV_EVENT_COMMAND_REPLY = 5
  integer(c_int), parameter :: MPV_EVENT_START_FILE = 6
  integer(c_int), parameter :: MPV_EVENT_END_FILE = 7
  integer(c_int), parameter :: MPV_EVENT_FILE_LOADED = 8
  integer(c_int), parameter :: MPV_EVENT_IDLE = 11

  type, bind(C) :: mpv_event
     integer(c_int) :: event_id
     integer(c_int) :: error
     type(c_ptr)    :: data
     integer(c_uint64_t) :: reply_userdata
  end type mpv_event

  interface
     function mpv_create() bind(C, name="mpv_create")
       import :: c_ptr
       type(c_ptr) :: mpv_create
     end function

     function mpv_initialize(ctx) bind(C, name="mpv_initialize")
       import :: c_ptr, c_int
       integer(c_int) :: mpv_initialize
       type(c_ptr), value :: ctx
     end function

     function mpv_command_string(ctx, cmd) bind(C, name="mpv_command_string")
       import :: c_ptr, c_char, c_int
       integer(c_int) :: mpv_command_string
       type(c_ptr), value :: ctx
       character(kind=c_char), dimension(*), intent(in) :: cmd
     end function

     function mpv_set_option_string(ctx, name, value) bind(C, name="mpv_set_option_string")
       import :: c_ptr, c_char, c_int
       integer(c_int) :: mpv_set_option_string
       type(c_ptr), value :: ctx
       character(kind=c_char), dimension(*), intent(in) :: name
       character(kind=c_char), dimension(*), intent(in) :: value
     end function

     function mpv_wait_event(ctx, timeout) bind(C, name="mpv_wait_event")
       import :: c_ptr, c_double
       type(c_ptr) :: mpv_wait_event
       type(c_ptr), value :: ctx
       real(c_double), value :: timeout
     end function

     subroutine mpv_terminate_destroy(ctx) bind(C, name="mpv_terminate_destroy")
       import :: c_ptr
       type(c_ptr) :: ctx
     end subroutine
  end interface
  !-------------------------------------------------------------------------

  ! helpers to pass C strings
contains
  function c_str(fstr) result(cptr)
    character(len=*), intent(in) :: fstr
    type(c_ptr) :: cptr
    character(kind=c_char), allocatable, target :: tmp(:)
    integer :: n

    n = len_trim(fstr)
    allocate(tmp(0:n))                 ! include null terminator
    if (n > 0) then
       call to_c_chars(fstr(1:n), tmp(0:n-1))
    end if
    tmp(n) = c_null_char
    cptr = c_loc(tmp(0))
  end function c_str

  subroutine to_c_chars(src, dst)
    character(len=*), intent(in) :: src
    character(kind=c_char), intent(out) :: dst(:)
    integer :: i
    do i = 1, len(src)
       dst(i-1) = transfer(src(i:i), dst(1))
    end do
  end subroutine to_c_chars

  function to_char_ptr(fstr) result(arr)
    character(len=*), intent(in) :: fstr
    character(kind=c_char), pointer :: arr(:)
    type(c_ptr) :: raw
    integer :: n
    n = len_trim(fstr)
    raw = c_str(fstr)
    call c_f_pointer(raw, arr, [n+1])
  end function to_char_ptr

  ! simple abort
  subroutine die(msg)
    character(len=*), intent(in) :: msg
    print *, 'ERROR: ', trim(msg)
    stop 1
  end subroutine die
!---------------------------------------------------------------------------
  ! main body
  implicit none
  type(c_ptr) :: ctx, ev_ptr
  type(mpv_event), pointer :: ev
  character(len=:), allocatable :: path
  character(kind=c_char), pointer :: cc(:)
  integer(c_int) :: rc
  real(c_double) :: timeout

  if (command_argument_count() < 1) then
     print *, 'Usage: ./fmpv path/to/video.mp4'
     stop 0
  end if
  call get_command_argument(1, length=len(path))
  allocate(character(len=len(path)) :: path)
  call get_command_argument(1, path)

  ctx = mpv_create()
  if (ctx .eq. c_null_ptr) call die("mpv_create returned NULL")

  ! Optional: set video output and terminal behavior
  cc => to_char_ptr("vo")
  rc = mpv_set_option_string(ctx, cc, to_char_ptr("gpu"))
  cc => to_char_ptr("terminal")
  rc = mpv_set_option_string(ctx, cc, to_char_ptr("yes"))
  cc => to_char_ptr("msg-level")
  rc = mpv_set_option_string(ctx, cc, to_char_ptr("all=v"))

  rc = mpv_initialize(ctx)
  if (rc /= 0_c_int) call die("mpv_initialize failed")

  ! Load and play the file
  rc = mpv_command_string(ctx, to_char_ptr("loadfile "//trim(path)))
  if (rc /= 0_c_int) call die("mpv_command_string(loadfile) failed")

  print *, "Playing: ", trim(path)
  print *, "Controls: close the window or press q in the mpv window."

  ! Event loop: wait until END_FILE or SHUTDOWN
  timeout = 1.0d0   ! seconds
  do
     ev_ptr = mpv_wait_event(ctx, timeout)
     if (ev_ptr .eq. c_null_ptr) cycle
     call c_f_pointer(ev_ptr, ev)

     select case (ev%event_id)
     case (MPV_EVENT_NONE)
        cycle
     case (MPV_EVENT_START_FILE)
        ! started
     case (MPV_EVENT_FILE_LOADED)
        ! ready
     case (MPV_EVENT_END_FILE, MPV_EVENT_SHUTDOWN)
        exit
     case default
        ! ignore others
     end select
  end do

  call mpv_terminate_destroy(ctx)
  print *, "Done."

end program mp4_player_mpv

3) Build & run

gfortran mp4_player_mpv.f90 -o fmpv -lmpv
./fmpv ~/Videos/sample.mp4

Notes

    This embeds mpv’s playback engine directly; no shelling out needed.

    Works with many formats (H.264/H.265/AV1, etc.) as long as your mpv/FFmpeg supports them.

    If you just want the absolute simplest approach with no C API, a one-liner fallback is:

    program play
      implicit none
      character(len=1024) :: f
      if (command_argument_count() < 1) stop "Usage: ./play file.mp4"
      call get_command_argument(1, f)
      call execute_command_line('mpv --fs "'//trim(f)//'"')
    end program

    Build with gfortran play.f90 -o play (requires mpv installed).

If you want a version using GStreamer instead, I can drop in a small C shim plus Fortran bindings next.


ChatGPT can make mistakes. Check important info.
