section .text
global our_code_starts_here
our_code_starts_here:
  mov eax, 0
  cmp eax, 0
  je if_false_1
  mov eax, 4
  jmp done_1
  if_false_1:
  mov eax, 2
  sub eax, 1
  done_1:
  ret
