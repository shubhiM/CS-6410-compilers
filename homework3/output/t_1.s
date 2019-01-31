section .text
global our_code_starts_here
our_code_starts_here:
  mov eax, 5
  mov [esp-4], eax
  mov eax, 4
  mov [esp-8], eax
  mov eax, 2
  mov [esp-12], eax
  mov eax, [esp-4]
  cmp eax, 0
  je if_false_12
  mov eax, [esp-8]
  jmp done_12
  if_false_12:
  mov eax, [esp-12]
  done_12:
  mov [esp-16], eax
  mov eax, [esp-16]
  ret
