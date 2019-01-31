section .text
global our_code_starts_here
our_code_starts_here:
  mov eax, 3
  mov [esp-4], eax
  mov eax, 4
  mov [esp-8], eax
  mov eax, 10
  mov [esp-12], eax
  mov eax, 8
  mov [esp-16], eax
  mov eax, [esp-16]
  ret
