section .text
global our_code_starts_here
our_code_starts_here:
  mov eax, 1
  cmp eax, 0
  je if_false_1
  mov eax, 2
  mov [esp-4], eax
  mov eax, [esp-4]
  add eax, 1
  mov [esp-8], eax
  mov eax, [esp-8]
  sub eax, 1
  jmp done_1
  if_false_1:
  mov eax, 3
  add eax, 1
  done_1:
  ret
