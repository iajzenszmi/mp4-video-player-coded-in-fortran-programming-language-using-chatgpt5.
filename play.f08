program play
  implicit none
  character(len=1024) :: f
  if (command_argument_count() < 1) stop "Usage: ./play file.mp4"
  call get_command_argument(1, f)
  call execute_command_line('mpv --fs "'//trim(f)//'"')
end program

