section .text
global our_code_starts_here
our_code_starts_here:
  mov eax, 1
  mov [esp-4], eax
  mov eax, [esp-4]
  ret
