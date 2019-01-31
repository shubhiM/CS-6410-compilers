section .text
global our_code_starts_here
our_code_starts_here:
  mov eax, 55
  cmp eax, 0
  je if_false_3
  mov eax, 1
  cmp eax, 0
  je if_false_5
  mov eax, 2
  add eax, 1
  jmp done_5
  if_false_5:
  mov eax, 3
  add eax, 1
  done_5:
  jmp done_3
  if_false_3:
  mov eax, 0
  cmp eax, 0
  je if_false_11
  mov eax, 4
  sub eax, 1
  jmp done_11
  if_false_11:
  mov eax, 5
  sub eax, 1
  done_11:
  done_3:
  mov [esp-4], eax
  mov eax, [esp-4]
  ret
