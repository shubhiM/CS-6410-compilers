section .text
global our_code_starts_here
our_code_starts_here:
  mov eax, 0
  mov [esp-4], eax
  mov eax, 4
  mov [esp-8], eax
  mov eax, 2
  mov [esp-12], eax
  mov eax, [esp-12]
  sub eax, 1
  mov [esp-16], eax
  mov eax, [esp-4]
  cmp eax, 0
  je if_false_16
  mov eax, [esp-8]
  jmp done_16
  if_false_16:
  mov eax, [esp-16]
  done_16:
  mov [esp-20], eax
  mov eax, [esp-20]
  ret
